mylistn(0, _, []).
mylistn(N, X, [X|Xs]) :- N>0, N1 is N - 1, mylistn(N1, X, Xs).

mydupn(_, [], []).
mydupn(N, [X|Xs], Ys) :- mylistn(N, X, NX), mydupn(N, Xs, NXs), append(NX, NXs, Ys).
