pack([], []).
pack([X], [[X]]).
pack([X, X|Ys], [[X|Xs]|Zs]) :- pack([X|Ys], [Xs|Zs]), !.
pack([X, Y|Xs], [[X]|Zs]) :- pack([Y|Xs], Zs).
