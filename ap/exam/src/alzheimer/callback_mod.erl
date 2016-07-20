-module(callback_mod).
-export([init/0, handle_read/2, handle_write/2]).

init() ->
% {error, something_went_wrong}.
 {ok, [1,2,3,4,5,6,7,8,9]}.

handle_read(Req, State) ->
%  throw(something_happened).
%  stop.
  {reply, lists:nth(Req, State)}.

handle_write(Req, State) ->
% throw(something_happened).
%  stop.
% {noUpdate, lists:sublist(State, Req-1) ++ [99] ++ lists:nthtail(Req, State)}.
  {updated, State, lists:sublist(State, Req-1) ++ [99] ++ lists:nthtail(Req, State)}.
