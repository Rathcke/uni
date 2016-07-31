-module(alzTests).
-include_lib("eunit/include/eunit.hrl").

query_test_() ->
  {_, P} = dbtest:start(),
  dbtest:insert(P, key1, value),
  timer:sleep(1),
  dbtest:insert(P, key2, value),
  timer:sleep(1),
  dbtest:insert(P, key3, value1),
  timer:sleep(1),
  dbtest:insert(P, key4, value1),
  timer:sleep(1),
  dbtest:insert(P, key5, value1),
  timer:sleep(1),
  dbtest:insert(P, key7, value1),
  timer:sleep(1),
  Rows1 = dbtest:getRows(P, value),
  timer:sleep(1),
  Rows2 = dbtest:getRows(P, value1),
  Rows3 = dbtest:getRows(P, value3),
  [ ?_assert(Rows1 == {ok,[{key1,value},{key2,value}]}),
    ?_assert(Rows2 == {ok,[{key3,value1},{key4,value1},{key5,value1},{key7,value1}]}),
    ?_assert(Rows3 == {ok,[]})
  ].

errors_test_() ->
  {_, P} = dbtest:start(),
  dbtest:insert(P, key1, value),
  timer:sleep(1),
  dbtest:insert(P, key2, value),
  timer:sleep(1),
  dbtest:insert(P, key3, value1),
  timer:sleep(1),
  dbtest:insert(P, key4, value1),
  timer:sleep(1),
  dbtest:insert(P, key5, value1),
  timer:sleep(1),
  dbtest:insert(P, key6, throw_error),
  timer:sleep(1),
  dbtest:insert(P, key7, value1),
  timer:sleep(1),
  RowErr1 = dbtest:getRowsThrows(P, value),
  [  ?_assert(RowErr1 == {error,{key6,throw_error}}),
     ?_assertThrow(error_new, dbtest:insertThrows(P, throw_error, val)),
     ?_assertThrow(error_existing, dbtest:insertThrows(P, key6, hello))
  ].
