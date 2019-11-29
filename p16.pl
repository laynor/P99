mydroph(_, _, [], []).
mydroph(1, N, [_|Xs], Ys) :- mydroph(N, N, Xs, Ys),!.
mydroph(M, N, [X|Xs], [X|Ys]) :- M1 is M-1, mydroph(M1, N, Xs, Ys).
mydrop(N, L, R) :- mydroph(N, N, L, R).
