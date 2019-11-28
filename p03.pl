element_at(X, [X|_], 1).
element_at(X, [_|Xs], N) :- N > 1, N2 is N-1, element_at(X, Xs, N2).
