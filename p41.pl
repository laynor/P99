:- ensure_loaded(p40).
writeLst(L) :- member(X, L), write(X), fail.
writeLst(_).

goldbach_list(N, M, Lim) :- 1 is N mod 2, N1 is N+1, goldbach_list(N1, M, Lim).
goldbach_list(N, M, Lim) :-
        N =< M,
        goldbach(N, [P1, P2]),
        (P1 > Lim, writeLst([N, ' = ', P1, ' + ', P2]), nl ; true),
        N1 is N+2, goldbach_list(N1, M, Lim).
goldbach_list(_, _, _).
