is_list([]).
is_list([_|_]).

flatten([], []).
flatten([Xs|Ys], Zs) :- is_list(Xs), append(Xs, Ys, Ws), flatten(Ws, Zs).
flatten([X|Xs], [X|Ys]) :- flatten(Xs, Ys).
