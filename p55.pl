cbalt(0, nil).
cbalt(1, t(x, nil, nil)).
cbalt(N, t(x, TL, TR)) :-
        N>1,
        Rest is N-1,
        0 is Rest mod 2,
        NHalf is Rest // 2,
        cbalt(NHalf, TL),
        cbalt(NHalf, TR).

cbalt(N, T) :-
        N > 1,
        Rest is N-1,
        1 is Rest mod 2,
        N1 is Rest//2,
        N2 is 1 + Rest//2,
        cbalt(N1, T1),
        cbalt(N2, T2),
        cbalth(T1, T2, T).

cbalth(T1, T2, t(x, T1, T2)).
cbalth(T1, T2, t(x, T2, T1)).