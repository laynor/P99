range(N, M, []) :- N > M.
range(N, M, [N|Xs]) :- N=<M, N1 is N+1, range(N1, M, Xs).
