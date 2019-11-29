
elLR(X, Xs, L, R) :- el(X, Xs, R), append(L, [X|R], Xs).

combr(0, Rc, [], Rc).
combr(N, Xs, [X|C], Rc) :-
        elLR(X, Xs, L, R),
        N1 is N-1,
        combr(N1, R, C, Rc1),
        append(L, Rc1, Rc).

group3(Xs, G1, G2, G3) :-
        combr(2, Xs, G1, R1),
        combr(3, R1, G2, R2),
        combr(4, R2, G3, []).

group(_, [], []).
group(Xs, [N|Ns], [G|Gs]) :-
        combr(N, Xs, G, R),
        group(R, Ns, Gs).
