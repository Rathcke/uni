:- consult('facein.pl').

:- begin_tests(facein).

%%% TEST GOODFRIENDS/3 %%%

test(goodfriendsFail1, [fail]) :-
  goodfriends([person(andrzej, [susan, ken]),
               person(ken, [andrzej]),
               person(susan, [reed, jessica, jen]),
               person(reed, [tony, jessica]),
               person(jessica, [jen]),
               person(tony, []),
               person(jen, [susan, jessica, tony])],
               tony, ken).

test(goodfriendsFail2, [fail]) :-
  goodfriends([person(andrzej, [susan, ken]),
               person(ken, [andrzej]),
               person(susan, [reed, jessica, jen]),
               person(reed, [tony, jessica]),
               person(jessica, [jen]),
               person(tony, []),
               person(jen, [susan, jessica, tony])],
               ken, james).

test(goodfriendsSucc1) :-
  goodfriends([person(andrzej, [susan, ken]),
               person(ken, [andrzej]),
               person(susan, [reed, jessica, jen]),
               person(reed, [tony, jessica]),
               person(jessica, [jen]),
               person(tony, []),
               person(jen, [susan, jessica, tony])],
               ken, andrzej).

test(goodfriendsSucc2) :-
  goodfriends([person(andrzej, [susan, ken]),
               person(ken, [andrzej]),
               person(susan, [reed, jessica, jen]),
               person(reed, [tony, jessica]),
               person(jessica, [jen]),
               person(tony, []),
               person(jen, [susan, jessica, tony])],
               jen, jessica).

%%% TEST CLIQUE/2 %%%

test(cliqueFail1, [fail]) :-
  clique([person(andrzej, [susan, ken]),
          person(ken, [andrzej]),
          person(susan, [reed, jessica, jen]),
          person(reed, [tony, jessica]),
          person(jessica, [jen]),
          person(tony, []),
          person(jen, [susan, jessica, tony])],
          [jen, tony]).

test(cliqueFail2, [fail]) :-
  clique([person(andrzej, [susan, ken]),
          person(ken, [andrzej]),
          person(susan, [reed, jessica, jen]),
          person(reed, [tony, jessica]),
          person(jessica, [jen]),
          person(tony, []),
          person(jen, [susan, jessica, tony])],
          [jen, jessica, susan]).

test(cliqueSucc1) :-
  clique([person(andrzej, [susan, ken]),
          person(ken, [andrzej]),
          person(susan, [reed, jessica, jen]),
          person(reed, [jen, jessica]),
          person(jessica, [jen, reed]),
          person(tony, []),
          person(jen, [susan, jessica, tony, reed])],
          [jen, reed, jessica]).

test(cliqueSucc2) :-
  clique([person(andrzej, [susan, ken]),
          person(ken, [andrzej]),
          person(susan, [reed, jessica, jen]),
          person(reed, [tony, jessica]),
          person(jessica, [jen, susan]),
          person(tony, []),
          person(jen, [susan, jessica, tony])],
          [jen, jessica]).

%%% TEST WANNABE/2 %%%
test(wannabeFail1, [fail]) :-
  wannabe([person(andrzej, [susan, ken]),
           person(ken, [andrzej]),
           person(susan, [reed, jessica, jen]),
           person(reed, [tony, jessica]),
           person(jessica, [jen]),
           person(tony, [jen]),
           person(jen, [susan, jessica, tony])],
           tony).

test(wannabeFail2, [fail]) :-
  wannabe([person(andrzej, [susan, ken]),
           person(ken, [andrzej]),
           person(susan, [reed, jessica, jen]),
           person(reed, [tony, jessica]),
           person(jessica, [jen]),
           person(tony, [jen]),
           person(jen, [susan, jessica, tony])],
           jen).

test(wannabeSucc1) :-
  wannabe([person(andrzej, [susan, ken]),
           person(ken, [andrzej]),
           person(susan, [reed, jessica, jen]),
           person(reed, [tony, jessica]),
           person(jessica, [jen]),
           person(tony, [jen]),
           person(jen, [susan, jessica, tony])],
           ken).

test(wannabeSucc2) :-
  wannabe([person(andrzej, [susan, ken]),
           person(ken, [andrzej]),
           person(susan, [reed, jessica, jen]),
           person(reed, [tony, jessica]),
           person(jessica, [jen]),
           person(tony, [jen]),
           person(jen, [susan, jessica, tony])],
           andrzej).

%%% TEST IDOL/2 %%%
test(idolFail1, [fail]) :-
  idol([person(andrzej, [susan, ken]),
           person(ken, [andrzej]),
           person(susan, [reed, jessica, jen]),
           person(reed, [tony, jessica]),
           person(jessica, [jen]),
           person(tony, [jen]),
           person(jen, [susan, jessica, tony])],
           andrzej).

test(idolFail2, [fail]) :-
  idol([person(andrzej, [susan, ken]),
           person(ken, [andrzej]),
           person(susan, [reed, jessica, jen]),
           person(reed, [tony, jessica]),
           person(jessica, [jen]),
           person(tony, [jen]),
           person(jen, [susan, jessica, tony])],
           ken).

test(idolSucc1) :-
  idol([person(andrzej, [susan, ken]),
           person(ken, [andrzej]),
           person(susan, [reed, jessica, jen]),
           person(reed, [tony, jessica]),
           person(jessica, [jen]),
           person(tony, [jen]),
           person(jen, [susan, jessica, tony])],
           jessica).

test(idolSucc2) :-
  idol([person(andrzej, [susan, ken]),
           person(ken, [andrzej]),
           person(susan, [reed, jessica, jen]),
           person(reed, [tony, jessica]),
           person(jessica, [jen]),
           person(tony, [jen]),
           person(jen, [susan, jessica, tony])],
           tony).

%%% TEST ISPATH/4 %%%
test(ispathFail1, [fail]) :-
  ispath([person(andrzej, [susan, ken]),
           person(ken, [andrzej]),
           person(susan, [reed, jessica, jen]),
           person(reed, [tony, jessica]),
           person(jessica, [jen]),
           person(tony, []),
           person(jen, [susan, jessica, tony])],
           andrzej, jessica, [andrzej, ->, susan, <-, jessica]).

test(ispathFail2, [fail]) :-
  ispath([person(andrzej, [susan, ken]),
           person(ken, [andrzej]),
           person(susan, [reed, jessica, jen]),
           person(reed, [tony, jessica]),
           person(jessica, [jen]),
           person(tony, []),
           person(jen, [susan, jessica, tony])],
           susan, andrzej, [susan, ->, andrzej]).

test(ispathFail3, [fail]) :-
  ispath([person(andrzej, [susan, ken]),
           person(ken, [andrzej]),
           person(susan, [reed, jessica, jen]),
           person(reed, [tony, jessica]),
           person(jessica, [jen]),
           person(tony, []),
           person(jen, [susan, jessica, tony])],
           andrzej, reed, [susan, ->, reed]).

 test(ispathFail4, [fail]) :-
  ispath([person(andrzej, [susan, ken]),
           person(ken, [andrzej]),
           person(susan, [reed, jessica, jen]),
           person(reed, [tony, jessica]),
           person(jessica, [jen]),
           person(tony, []),
           person(jen, [susan, jessica, tony])],
           andrzej, reed, [andrzej, ->, susan, ->, jessica]).


 test(ispathFail5, [fail]) :-
  ispath([person(andrzej, [susan, ken]),
           person(ken, [andrzej]),
           person(susan, [reed, jessica, jen]),
           person(reed, [tony, jessica]),
           person(jessica, [jen]),
           person(tony, []),
           person(jen, [susan, jessica, tony])],
           andrzej, reed, [andrzej, ->, susan, ->, reed, ->, tony]).

 test(ispathSucc1) :-
  ispath([person(andrzej, [susan, ken]),
           person(ken, [andrzej]),
           person(susan, [reed, jessica, jen]),
           person(reed, [tony, jessica]),
           person(jessica, [jen]),
           person(tony, []),
           person(jen, [susan, jessica, tony])],
           andrzej, reed, [andrzej, ->, susan, ->, reed]).

 test(ispathSucc2) :-
  ispath([person(andrzej, [susan, ken]),
           person(ken, [andrzej]),
           person(susan, [reed, jessica, jen]),
           person(reed, [tony, jessica]),
           person(jessica, [jen]),
           person(tony, []),
           person(jen, [susan, jessica, tony])],
           reed, andrzej, [reed, <-, susan, <-, andrzej]).

 test(ispathSucc3) :-
  ispath([person(andrzej, [susan, ken]),
           person(ken, [andrzej]),
           person(susan, [reed, jessica, jen]),
           person(reed, [tony, jessica]),
           person(jessica, [jen]),
           person(tony, []),
           person(jen, [susan, jessica, tony])],
           andrzej, reed, [andrzej, ->, susan, ->, reed, ->, tony, <-, reed]).

h:- end_tests(facein).
