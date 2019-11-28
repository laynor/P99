rev(L, R) :- rev2(L, [], R).
rev2([], R, R) :- !.
rev2([X|Xs], Acc, R) :- rev2(Xs, [X|Acc], R).
