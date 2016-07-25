%%%% HELPER FUNCTIONS %%%

/* Negates a boolean value */
negate(X) :-
  X -> false; true.

/* Determines if X is in list [Y|YS] */
inList(X, [X|_]).
inList(X, [_|YS]) :- inList(X, YS).

/* Determines if there is a path from a given person to all other persons in G */
findP(_, _, _, _, []).
findP(X, X2, V, G, [Y|YS]) :-
  inList(person(X, _), [Y]), findP(X2, X2, [], G, YS); % Person X is equal to Y
  inList(person(X, L), G), % Find a person in X's friendlist
  inList(T, L),
  negate(inList(T, V)), % Person is not already visited
  (inList(person(T, _), [Y]), findP(X2, X2, [], G, YS); % Person Y was in the friendlist
  findP(T, X, [T|V], G, [Y|YS])). % Person was not in friendlist, recursive call

/* Determines if there is a path from all persons in G to some person X */
pathP(_, _, []).
pathP(X, G, [Y|YS]) :-
  inList(person(Z, _), [Y]),
  findP(Z, Z, [Z], G, [person(X, _)]), % There is a path from person Y to X
  pathP(X, G, YS).

/* Confirms a given path between two persons is possible */
confirmPath(_, Y, [Y]).
confirmPath(G, Y, [Xi, ->, Y|[]]) :-
  inList(person(Xi, Zi), G),
  inList(Y, Zi).
confirmPath(G, Y, [Xi, <-, Y|[]]) :-
  inList(person(Y, Zi), G),
  inList(Xi, Zi).
confirmPath(G, Y, [Xi, ->, Xj|P]) :-
  inList(person(Xi, Zi), G),
  inList(Xj, Zi),
  confirmPath(G, Y, [Xj|P]).
confirmPath(G, Y, [Xi, <-, Xj|P]) :-
  inList(person(Xj, Zj), G),
  inList(Xi, Zj),
  confirmPath(G, Y, [Xj|P]).

%%% END HELPER FUNCTIONS %%%

/* Determines if person X and Y exists and has each other in the friendlist */
goodfriends(G, X, Y) :-
  inList(person(X, Z), G),
  inList(Y, Z), % Person Y is in X's friendlist
  inList(person(Y, Z2), G),
  inList(X, Z2). % Person X is in Y's friendlist

/* Determines if a group of people are pairwise all good friends (a clique) */
clique(G, [X, Y]) :-
  goodfriends(G, X, Y).
clique(G, [X, Y|YS]) :-
  goodfriends(G, X, Y),
  clique(G, [X|YS]), % Check person X is goodfriends with the entire list
  clique(G, [Y|YS]). % Check the next person is goodfriends with the entire list

/* Determines if there is a path from a person X to all other persons in G */
wannabe(G, X) :-
  findP(X, X, [X], G, G).

/* Determines if there is a path from all person in G to person X */
idol(G, X) :-
  pathP(X, G, G).

/* Determines if a given path P from X to Y is possible */
ispath(G, X, Y, [X|P]) :-
  confirmPath(G, Y, [X|P]).
