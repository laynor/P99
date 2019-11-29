consEach(_, [], []).
consEach(X, [Y|Ys], [Z|Zs]) :- atom_concat(X, Y, Z), consEach(X, Ys, Zs).

gray(1, ['0', '1']).
gray(N, Cs) :-
        N>1,
        N1 is N-1,
        gray(N1, T),
        reverse(T, RT),
        consEach('0', T, T1),
        consEach('1', RT, RT1),
        append(T1, RT1, Cs).
