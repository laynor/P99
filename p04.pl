% len(N, L).
len(0, []).
len(N, [_|Xs]) :- len(N1, Xs), N is N1 + 1.
