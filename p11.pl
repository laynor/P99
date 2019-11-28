:- ensure_loaded(p10).
rleComp([], []).
rleComp([[1, X]|Ys], [X|Zs]) :- rleComp(Ys, Zs), !.
rleComp([X|Xs], [X|Ys]) :- rleComp(Xs, Ys).
rlemod(L1, Enc) :- rle(L1, Enc1), rleComp(Enc1, Enc).
