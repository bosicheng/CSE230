sorted([]).
sorted([X]).
sorted([X,Y|T]) :- X=<Y, sorted([Y|T]).

sort(X, Y) :- permutation(X, Y), sorted(Y), !.

split([], [], []).
split([X], [X], []).
split([X | T], [X|T1], T2) :- split(T,T2,T1).

