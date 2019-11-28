my_last(X, [X]).
my_last(Y, [_|Xs]) :- my_last(Y, Xs).
