-module(gen_replicated).
-export([start/2, stop/1, read/2, write/2, coordinator/1, replica/2, get_pids/1]).

%%%=========================================================================
%%%  API
%%%=========================================================================

start(NumReplica, _) when NumReplica < 2 -> {error, "NumReplica < 2"};
start(NumReplica, Mod) ->
  case Mod:init() of
    {ok, _}         -> ServerRef = spawn_link(fun() -> coordinator([]) end),
                       ServerRef ! {make_replicas, NumReplica, Mod},
                       {ok, ServerRef};
    {error, Reason} -> {error, Reason}
  end.

stop(Server) ->
  Server ! {self(), stop},
  receive
    {stopped, _} -> {ok, stopped}
  end.

read(Server, Req) ->
  Server ! {self(), {read, Req}},
  receive
    {ok, Result} -> {ok, Result};
    stop         -> stop(Server);
    {throw, Val} -> {'ABORTED', exception, Val}
  end.

write(Server, Req) ->
  blocking(Server, Req).

get_pids(Server) ->
  Server ! {self(), getPids},
  receive
    {ok, Pids} -> {ok, Pids}
  end.

%Helper Functions
blocking(Server, Req) ->
  Server ! {self(), {write, Req}},
  receive
    {ok, Reply}           -> {ok, Reply};
    {ok, Reply, NewState} -> {ok, Reply, NewState};
    stop                  -> stop(Server);
    {throw, Val}          -> {'Aborted', exception, Val}
  end.

createReplica(NumReplica, _) when NumReplica == 0 -> [];
createReplica(NumReplica, Mod) ->
  {_, State} = Mod:init(),
  ReplicaRef = spawn_link(fun() -> replica(State, Mod) end),
  [{ReplicaRef, free} | createReplica(NumReplica-1, Mod)].

%findAvailable(N, _) when N == 1 -> fail;
%findAvailable(N, Pids) ->
%  case element(2, lists:nth(N, Pids)) of
%    free -> element(1, lists:nth(N, Pids));
%    busy -> findAvailable(N-1, Pids)
%  end.

%Coordinator and Replicas
coordinator(Pids) ->
  receive
    {From, getPids} ->
      From ! {ok, Pids},
      coordinator(Pids);
    {make_replicas, NumReplica, Mod} ->
      NewPids = createReplica(NumReplica, Mod),
      coordinator(NewPids);
    {From, {read, Req}} ->
      Reader = element(1, lists:nth(1+random:uniform(length(Pids)-1), Pids)),
      Reader ! {From, {read, Req}},
      coordinator(Pids);
    {From, {write, Req}} ->
      Reader = element(1, lists:nth(1, Pids)),
      Reader ! {self(), From, {write, Req}},
      coordinator(Pids);
    {update, NewState} ->
      Msg = {update, NewState},
      [element(1, Replica) ! Msg || Replica <- Pids],
      coordinator(Pids);
    {From, stop} ->
      Msg = {From, stop},
      [element(1, Replica) ! Msg || Replica <- Pids]
  end.

replica(State, Mod) ->
  receive
    {From, {read, Req}} ->
      try Mod:handle_read(Req, State) of
        {reply, Reply} -> From ! {ok, Reply},
                          replica(State, Mod);
        stop           -> From ! stop
      catch
        throw : Val    -> From ! {throw, Val}
      end;
    {Coordinator, From, {write, Req}} ->
      try Mod:handle_write(Req, State) of
        {noUpdate, Reply}          -> From ! {ok, Reply},
                                      replica(State, Mod);
        {updated, Reply, NewState} -> Coordinator ! {update, NewState},
                                      From ! {ok, Reply, NewState},
                                      replica(NewState, Mod);
        stop                       -> From ! stop
      catch
        throw : Val                -> From ! {throw, Val}
      end;
    {update, UpdatedState} ->
      replica(UpdatedState, Mod);
    {From, stop} ->
      From ! {stopped, self()}
  end.
