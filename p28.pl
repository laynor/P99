% P28 -- sorting

msort([], []) :- !.
msort([X], [X]) :- !.
msort(Xs, Sorted) :-
        length(Xs, Len),
        Len > 1,
        HalfLen is ceiling(Len/2),
        split(Xs, HalfLen, Left, Right),
        msort(Left, LeftSorted),
        msort(Right, RightSorted),
        merge(LeftSorted, RightSorted, Sorted).


merge([], Ys, Ys) :- !.
merge(Xs, [], Xs) :- !.
merge([X|Xs], [Y|Ys], [X|Zs]) :-
        X =< Y,
        merge(Xs, [Y|Ys], Zs).

merge([X|Xs], [Y|Ys], [Y|Zs]) :-
        X > Y,
        merge([X|Xs], Ys, Zs).

lsort([], []) :- !.
lsort([X], [X]) :- !.
lsort(Xs, Sorted) :-
        length(Xs, Len),
        Len > 1,
        HalfLen is ceiling(Len/2),
        split(Xs, HalfLen, Left, Right),
        lsort(Left, LeftSorted),
        lsort(Right, RightSorted),
        lmerge(LeftSorted, RightSorted, Sorted).

lmerge([], Ys, Ys) :- !.
lmerge(Xs, [], Xs) :- !.
lmerge([X|Xs], [Y|Ys], [X|Zs]) :-
        length(X, Lx),
        length(Y, Ly),
        Lx =< Ly,
        lmerge(Xs, [Y|Ys], Zs).

lmerge([X|Xs], [Y|Ys], [Y|Zs]) :-
        length(X, Lx),
        length(Y, Ly),
        Lx > Ly,
        lmerge([X|Xs], Ys, Zs).



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



lens([], []).
lens([X|Xs], [L|Ls]) :- length(X, L), lens(Xs, Ls).

zip([], [], []).
zip([X|Xs], [Y|Ys], [[X,Y]|Zs]) :- zip(Xs, Ys, Zs).

withLens(Xs, Ys) :- lens(Xs, Ls), zip(Ls, Xs, Ys).

groupk([], nil, _, []).
groupk([], K, Vs, [[K, Vs]]).
groupk([[K, V]|KVs], K, Vs, Gs) :- groupk(KVs, K, [V|Vs], Gs).
groupk([[K1, V]|KVs], K2, Vs, [[K2, Vs]|Gs]) :- K1 \= K2, groupk([[K1, V]|KVs], K1, [], Gs).

values([], []).
values([[_,V]|KVs], [V|Vs]) :- values(KVs, Vs).

concat([], []).
concat([X|Xs], Ys) :- concat(Xs, Zs), append(X, Zs, Ys).

lfsort(Xs, Ys) :-
        withLens(Xs, LXs),
        mksort(LXs, [[L, X]|SLXs]),
        groupk(SLXs, L, [X], Gs),
        values(Gs, ByLen),
        withLens(ByLen, FreqTagged),
        mksort(FreqTagged, FreqSorted),
        values(FreqSorted, Zs),
        concat(Zs, Ys).
