:- ensure_loaded(p17).
removek(Xs, X, K, Ys) :- K1 is K-1, split(Xs, K1, L, [X|R]), append(L, R, Ys).
