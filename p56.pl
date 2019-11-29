mirror(nil, nil).
mirror(t(_, A, B), t(_, X, Y)) :- mirror(A, Y), mirror(B, X).

symmetric(nil).
symmetric(t(_, A, B)) :- mirror(A, B).