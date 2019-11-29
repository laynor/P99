split(L, 0, [], L).
split([X|Xs], N, [X|L1], L2) :- N1 is N-1, split(Xs, N1, L1, L2).

split2(Xs, N, L, R) :- length(L, N), append(L, R, Xs).
