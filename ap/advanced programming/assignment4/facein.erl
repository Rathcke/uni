-module(facein).
-export([start/1, add_friend/2, friends/1, broadcast/3, received_messages/1]).

start(Name) ->
  Pid = keep_looping(Name),
  {ok, Pid}.

add_friend(Pid, Fid) ->
  if
    Pid =:= Fid ->
      Pid ! {error, cannot_add_self_to_fl};
    true ->
      rpc(Pid, {add_friend, Fid})
  end.

friends(Pid) ->
  rpc(Pid, friends).

give_info(Pid) ->
  rpc(Pid, give_info).

broadcast(Pid, Msg, Radius) ->
  Ref = make_ref(),
  {_, Name} = Pid ! give_info(Pid),
  Pid ! {self(), Ref, {broadcast, Msg, Radius, Name}}.

received_messages(Pid) ->
  rpc(Pid, received_messages).

rpc(Pid, Request) ->
  Ref = make_ref(),
  Pid ! {self(), Ref, Request},
  receive
    {Ref, Response} -> Response
  end.

keep_looping(Name) ->
  spawn(fun() ->
                process_flag(trap_exit, true),
                Friends = dict:new(),
                Msgs = dict:new(),
                Worker = spawn_link(fun() -> loop(Name, Msgs, Friends) end),
                supervisor(Name, Msgs, Worker)
        end).

supervisor(Name, Msgs, Worker) ->
  receive
    {'EXIT', Worker, Reason} ->
      io:format("~p exited because of ~p~n",[Worker, Reason]),
      Msgs1 = dict:new(),
      Worker1 = spawn_link(fun() -> loop(Name, Msgs1, dict:new()) end),
      supervisor(Name, Msgs1, Worker1);
    Msg ->
      Worker ! Msg,
      supervisor(Name, Msgs, Worker)
  end.

loop(Name, Msgs, Friends) ->
  receive
    {From, Ref, {add_friend, Fid}} ->
      {_, Fname} = Fid ! give_info(Fid),
      case dict:is_key(Fid, Friends) of
        false ->
          From ! {Ref, ok},
          loop(Name, Msgs, dict:store(Fid, Fname, Friends));
        true ->
          From ! {Ref, {error, Fname, is_already_a_friend}},
          loop(Name, Msgs, Friends)
      end;
    {From, Ref, give_info} ->
      From ! {Ref, {ok, Name}},
      loop(Name, Msgs, Friends);
    {From, Ref, friends} ->
      List = dict:to_list(Friends),
      From ! {Ref, {ok, lists:map(fun({_, FN}) -> FN end, List)}},
      loop(Name, Msgs, Friends);
    {_, Ref, {broadcast, Msg, 0, OrigName}} ->
      %% CHECK IF MSG IS ALREADY ADDED
      case dict:is_key(Ref, Msgs) of
            false ->
              %OriginalSender = From ! give_info(From),
              loop(Name, dict:store(Ref, {OrigName, Msg}, Msgs), Friends);
            true ->
              loop(Name, Msgs, Friends)
      end;
    {From, Ref, {broadcast, Msg, Radius, OrigName}} ->
      %% ADD MESSAGE TO MESSAGES IF IT HAVE NOT BEEN DONE
      case dict:is_key(Ref, Msgs) of
            false ->
              Fids = lists:map(fun({Fid, _}) -> Fid end, dict:to_list(Friends)), % Get friend-list
              send_broadcast(From, Ref, Msg, Radius, Fids, OrigName), % Send to friends
              loop(Name, dict:store(Ref, {OrigName, Msg}, Msgs), Friends);
            true ->
              loop(Name, Msgs, Friends)
      end;
    {From, Ref, received_messages} ->
      From ! {Ref, lists:map(fun({_,Msg}) -> Msg end, dict:to_list(Msgs))},
      loop(Name, Msgs, Friends)
  end.

send_broadcast(_, _, _, _, [], _) -> ok;
send_broadcast(From, Ref, Msg, Radius, [Fid|T], OrigName) ->
  Fid ! {From, Ref, {broadcast, Msg, Radius - 1, OrigName}},
  send_broadcast(From, Ref,Msg, Radius, T, OrigName).
