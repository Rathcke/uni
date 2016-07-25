-module(facein_test).
-include_lib("eunit/include/eunit.hrl").

minimal_test_() ->
  {_, Ken} = facein:start("Ken Friis Larsen"),
  {_, Andrzej} = facein:start("Andrzej Filinski"),
  {_, Susan} = facein:start("Susan Storm"),
  {_, Reed} = facein:start("Reed Richards"),
  {_, Jessica} = facein:start("Jessica Drew"),
  {_, Jen} = facein:start("Jen Walters"),
  {_, Tony} = facein:start("Tony Stark"),
  facein:add_friend(Ken, Andrzej),
  facein:add_friend(Andrzej, Ken),
  facein:add_friend(Andrzej, Susan),
  facein:add_friend(Susan, Andrzej),
  facein:add_friend(Susan, Reed),
  facein:add_friend(Susan, Jessica),
  facein:add_friend(Susan, Jen),
  facein:add_friend(Jen, Susan),
  facein:add_friend(Jen, Tony),
  facein:add_friend(Jen, Jessica),
  facein:add_friend(Jessica, Jen),
  facein:add_friend(Reed, Jessica),
  facein:add_friend(Reed, Tony),
  facein:broadcast(Jessica, "message", 2),
  facein:broadcast(Ken, "another message", 3),
  [?_assert(facein:friends(Susan) =:= {ok, ["Andrzej Filinski",
                                             "Reed Richards",
                                             "Jessica Drew",
                                             "Jen Walters"]}),
   ?_assert(facein:received_messages(Susan) == [{"Jessica Drew","message"},
                                                  {"Ken Friis Larsen","another message"}]),
   ?_assert(facein:received_messages(Tony) == [{"Jessica Drew","message"}]),
   ?_assert(facein:received_messages(Reed) == [{"Ken Friis Larsen","another message"}])
  ].

errors_test_() ->
  {_, Ken} = facein:start("Ken Friis Larsen"),
  {_, Andrzej} = facein:start("Andrzej Filinski"),
  facein:add_friend(Ken, Andrzej),
  [?_assert(facein:add_friend(Ken, Ken) == {error, cannot_add_self_to_fl}),
   ?_assert(facein:add_friend(Ken, Andrzej) == {error, "Andrzej Filinski", is_already_a_friend})
  ].
