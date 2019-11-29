mergek([], Ys, Ys) :- !.
mergek(Xs, [], Xs) :- !.
mergek([X|Xs], [Y|Ys], [X|Zs]) :-
        X = [Kx, _],
        Y = [Ky, _],
        Kx =< Ky,
        mergek(Xs, [Y|Ys], Zs).

mergek([X|Xs], [Y|Ys], [Y|Zs]) :-
        X = [Kx, _],
        Y = [Ky, _],
        Kx > Ky,
        mergek([X|Xs], Ys, Zs).

mksort([], []) :- !.
mksort([X], [X]) :- !.
mksort(Xs, Sorted) :-
        length(Xs, Len),
        Len > 1,
        HalfLen is ceiling(Len/2),
        split(Xs, HalfLen, Left, Right),
        mksort(Left, LeftSorted),
        mksort(Right, RightSorted),
        mergek(LeftSorted, RightSorted, Sorted).

huffman(Fs, Hs).
