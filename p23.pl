:- ensure_loaded(p20).
rnd_select([], _, []).
rnd_select(_, 0, []).
rnd_select(L, N, [X|Xs]) :-
        N > 0,
        length(L, Len),
        random(1, Len, I1),
        removek(L, X, I1, L1),
        N1 is N-1,
        rnd_select(L1, N1, Xs).
