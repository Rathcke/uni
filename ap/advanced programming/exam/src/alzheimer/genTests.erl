-module(genTests).
-export([concurrent_reads_sleep/3]).
-include_lib("eunit/include/eunit.hrl").

basic_rw_test_() -> 
  {_, C} = replica:start(4),
  L1 = replica:read(C, tolist),
  replica:write(C, {add_to_dict, key1, value1}),
  L2 = replica:read(C, tolist),
  replica:write(C, {add_to_dict, key2, value2}),
  replica:write(C, {add_to_dict, key2, value3}),
  L3 = replica:read(C, tolist),
  [ ?_assert(L1 == []),
    ?_assert(L2 == [{key1, [value1]}]),
    ?_assert(L3 == [{key1, [value1]},{key2,[value2]}])
  ].
concurrent_read_test_() -> 
  {_, C} = replica:start(5),
  replica:write(C, {add_to_dict, key1, value1}),
  replica:write(C, {add_to_dict, key2, value2}),
  timer:exit_after(3000, time_out),
  L = concurrent_reads_sleep(C, 4, 2000),
  [ ?_assert( L == [[{key1, [value1]},{key2,[value2]}],
                    [{key1, [value1]},{key2,[value2]}],
                    [{key1, [value1]},{key2,[value2]}],
                    [{key1, [value1]},{key2,[value2]}]])
  ].


concurrent_reads_sleep(Coordinator,N,Time) ->
    Self = self(),
    Pids = [ spawn_link(fun() -> Self ! {self(), replica:read(Coordinator, {tolist,Time})} end)
                         || _X <- lists:seq(1,N) ],
    [ receive {Pid, R} -> R end || Pid <- Pids ].
