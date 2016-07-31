-module(gen_replicated).
-export([start/2, stop/1, read/2, write/2, extractRepl/1]).

%%%=========================================================================
%%%  API
%%%=========================================================================

-callback init() ->
    {ok, State :: term()}.
-callback handle_read(Request :: term(), State :: term()) ->
    {reply, Reply :: term()} |
    stop.
-callback handle_write(Request :: term(), State :: term()) ->
    {noupdate, Reply :: term()} |
    {update, Reply :: term()} |
    stop.

%%% STARTING A COORDINATOR
start(NumReplica, Mod) ->
    {ok, spawn(fun() ->
                 Readers = 0,
                 Writing = false,
                 {_ ,InitState} = Mod:init(),
                 Replicators = spawn_replicators(NumReplica, Mod, self()),
                 coordinator(Replicators, Readers, Writing, Mod, InitState)
               end)}.

stop(Server) ->
    Server ! stop.

read(Server, Req) ->
    Server ! {self(), {begin_read, Req}},
    receive
      {_From, Reply} -> Reply
    end.

write(Server, Req) ->
    Server ! {self(), {begin_write, Req}},
    receive
      {_From, Reply} -> Reply
    end.

%% Recursively spawn desired amount of workers
spawn_replicators(0, _, _) -> [];
spawn_replicators(N, Mod, Coordinator) ->
    {_, State} = Mod:init(),
    [spawn(fun() -> replica(Coordinator, Mod, State) end) | spawn_replicators(N-1, Mod, Coordinator)].

%%% INTERNAL LOGIC

extractRepl([]) -> [];
extractRepl([R|Rs]) ->
    {R, Rs}.

%% Send new copy of updated state to all replicas
updateReplicas([], _Newstate) -> ok;
updateReplicas([Repl | Rest], NewState) ->
    Repl ! {replicate, NewState},
    updateReplicas(Rest, NewState).

%% Send stop request to all replicas
terminateReplicas([]) -> ok;
terminateReplicas([Repl | Rest]) ->
    Repl ! stop,
    terminateReplicas(Rest).

coordinator(Replicators, Readers, Writing, Mod, State) ->
    receive
        {From, {begin_read, Request}} ->
            {Reader, Rest} = extractRepl(Replicators),
            %% If writing, add reader to queue with request and loop again
            %% When writing is done, check if there is a queue and execute those read commands
            Reader ! {self(), {read, Request, From}},
            coordinator(Rest, Readers + 1, Writing, Mod, State);
        {From, {begin_write, Request}} ->
            % Pick replica and assign to writer.
            {Writer, Rest} = extractRepl(Replicators),
            % Send write request to reader
            Writer ! {self(), {write, Request}},
            getWriter(From, Readers, Writing, Mod, State, [Writer| Rest]);
        {From, {done_read, Data, Forward}} ->
            Forward ! {self(), Data},
            coordinator([From | Replicators], Readers - 1, Writing, Mod, State);
        {_From, {done_replicating}} ->
            % Count number of replicas that are done and open up for reading when all are.
            undefined;
	      stop ->
            terminateReplicas(Replicators),
            Mod:terminate(stop_requested, State)
    end.

getWriter(From, Readers, Writing, Mod, State, [Writer | Rest]) ->
    receive
        {Writer, {done_writing, Reply, NewState}} -> % Update
            % Replicate NewState to all Replicas
            updateReplicas(Rest, NewState),
            % Send back reply
            From ! {self(), Reply},
            coordinator([Writer | Rest], Readers, Writing = false, Mod, NewState);
        {Writer, {done_writing, Reply}} -> %% No update
            % Send back reply
            From ! {self(), Reply},
            coordinator([Writer | Rest], Readers, Writing = false, Mod, State);
        {From, _} ->
            From ! {self(), {busy_writing}},
            getWriter(From, Readers, Writing, Mod, State, [Writer | Rest])
            % HANDLE INCOMMING READ REQUESTS
    end.

replica(Coordinator, Mod, State)->
    receive
         {_From, {read, Request, Forward}} ->
              io:format("repl read req:~w ~n",[self()]),
              case Mod:handle_read(Request, State) of
                  {reply, Reply} ->
                      Coordinator ! {self(), {done_read, Reply, Forward}},
                      replica(Coordinator, Mod, State);
                  stop ->
                      Coordinator ! stop,
                      replica(Coordinator, Mod, State)
              end;
         {_From, {write, Request}} ->
            case Mod:handle_write(Request, State) of
              {noupdate, Reply} ->
                  Coordinator ! {self(), {done_writing, Reply}},
                  replica(Coordinator, Mod, State);
              {updated, Reply, NewState} ->
                  Coordinator ! {self(), {done_writing, Reply, NewState}},
                  replica(Coordinator, Mod, NewState);
              stop ->
                  Coordinator ! stop,
                  replica(Coordinator, Mod, State)
            end;
        {replicate, Copy} ->
            %Coordinator ! {self(), {done_replicating}},
            replica(Coordinator, Mod, Copy);
        stop ->
            Mod:terminate(stop_requested, State)
   end.
