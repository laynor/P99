:- ensure_loaded(p26).
el(X, [X|R], R).
el(X, [_|L], R) :- el(X, L, R).

comb(0, _, []).
comb(N, L, [X|C]) :-
        el(X, L, R),
        N1 is N-1,
        comb(N1, R, C).
