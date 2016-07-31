-module(replica).
-behaviour(gen_replicated).
%-import(gen_replicated, []).
-export([start/1, write/2, read/2, stop/2]).
-export([init/0, handle_read/2, handle_write/2, terminate/2]).

%% Callback functions
start(Replicas) ->
  gen_replicated:start(Replicas, ?MODULE).

write(Coordinator, Req) ->
  gen_replicated:write(Coordinator, Req).

read(Coordinator, Req) ->
  gen_replicated:read(Coordinator, Req).

stop(Coordinator) ->
  gen_replicated:stop(Coordinator).

init() ->
  {ok, dict:new()}.

terminate(_Reason, State) ->
    dict:delete(State).

handle_read(tolist, State) ->
  {reply, dict:to_list(State)};
handle_read({tolist, Time}, State) ->
  timer:sleep(Time),
  {reply, dict:to_list(State)}.

handle_write({add_to_dict,K,V}, State) ->
  case dict:is_key(K, State) of
      false ->
          {updated , key_added_to_dict, dict:append(K, V, State)};
      true  -> 
          {noupdate , key_is_in_dictionary}
  end;
handle_write({add_to_dict,K,V,Time}, State) ->
  timer:sleep(Time),
  case dict:is_key(K, State) of
      false ->
          {updated , key_added_to_dict, dict:append(K, V, State)};
      true  -> 
          {noupdate , key_is_in_dictionary}
  end.

