:- ensure_loaded(p09).
rleh([], []).
rleh([[X|Xs]|Ys], [[N, X]|Zs]) :- length([X|Xs], N), rleh(Ys, Zs).

rle(L1, Encoded) :- pack(L1, Packed), rleh(Packed, Encoded).
