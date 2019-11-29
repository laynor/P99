:- ensure_loaded(p17).
rotln(Xs, N, Ys) :- N>=0, split(Xs, N, L, R), append(R, L, Ys).
rotln(Xs, N, Ys) :- N<0, length(Xs, L), N1 is L+N, rotln(Xs, N1, Ys).
