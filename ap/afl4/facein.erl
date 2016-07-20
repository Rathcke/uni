-module(facein).
-compile(export_all).

start(Name) ->
  Pid = keep_looping(Name),
  {ok, Pid}.

add_friend(Pid, Fid) ->
  rpc(Pid, {add_friend, Fid}).

friends(Pid) ->
  rpc(Pid, friends).

give_info(Pid) ->
  rpc(Pid, give_info).

broadcast(Pid, Msg, Radius) ->
  Ref = make_ref(),
  Pid ! {self(), Ref, {broadcast, Msg, Radius}}.

rpc(Pid, Request) ->
  Ref = make_ref(),
  Pid ! {self(), Ref, Request},
  receive
    {Ref, Response} -> Response
  end.

p_serv(Name, Friends, Msgs) ->
  receive
    {From, Ref, {add_friend, Fid}} ->
      Response = Fid ! facein:give_info(Fid),
      case Response of
        {ok, Fname} ->
          From ! {Ref, ok},
          p_serv(Name, dict:store(Fid, Fname, Friends), Msgs);
        {_, _} ->
          From ! {Ref, {error, Fid, something_went_wrong}},
          p_serv(Name, Friends, Msgs)
      end;
    {From, Ref, give_info} ->
      From ! {Ref, {ok, Name}},
      p_serv(Name, Friends, Msgs);
    {From, Ref, friends} ->
      List = dict:to_list(Friends),
      From ! {Ref, {ok, lists:map(fun({_, FN}) -> FN end, List)}},
      p_serv(Name, Friends, Msgs);
    {From, Ref, {broadcast, Msg, Radius}} ->
      All = list:append([[self()], friend_range(Friends, Radius)]),
      Set = sets:from_list(All),
      sets:to_list(Set),
      Ref = make_ref(),
      lists:map(fun(Fid) -> Fid ! {self(), Ref, {get_msg, Msg}} end, All),
      p_serv(Name, Friends, Msgs);
    {From, Ref, {get_msg, Msg}} ->
      New_msgs= [Msg|Msgs],
      p_serv(Name, Friends, New_msgs)
  end.

friend_range([], _) -> [];
friend_range(_, Radius) when Radius == 0 -> [];
friend_range([X|XS], Radius) when Radius > 0 ->
  {_, F_list} = rpc(X, friends).
%  lists:append([[X], friend_range([XS], Radius), friend_range(F_list, Radius - 1)]).

keep_looping(Name) ->
  spawn(fun () ->
    process_flag(trap_exit, true),
    Friends = dict:new(),
    Msgs = dict:new(),
    Worker = spawn_link(fun() -> p_serv(Name, Friends, Msgs) end),
    supervisor(Name, Worker)
  end).

supervisor(Name, Worker) ->
    receive
        {'EXIT', Worker, Reason} ->
            io:format("~p exited because of ~p~n", [Worker, Reason]),
            Worker1 = spawn_link(fun() -> p_serv(Name, dict:new(), dict:new()) end),
            supervisor(Name, Worker1);
        Msg ->
            Worker ! Msg,
            supervisor(Name, Worker)
    end.
