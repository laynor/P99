:- ensure_loaded(p11).
rle1([], []).
rle1([X], [[1,X]]).
rle1([X, X|Ys], [[N, X]|Zs]) :- rle1([X|Ys], [[N1, X]|Zs]), N is N1 + 1, !.
rle1([X, Y|Xs], [[1, X]|Zs]) :- rle1([Y|Xs], Zs).
rledir(L1, Enc) :- rle1(L1, Enc1), rleComp(Enc1, Enc).
