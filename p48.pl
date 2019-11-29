:- ensure_loaded(p47).
bindList([]).
bindList([X|Xs]) :- bind(X), bindList(Xs).

intersp([], _, []).
intersp([X], _, [X]) :- !.
intersp([X|Xs], Y, [X, Y|Ys]) :- intersp(Xs, Y, Ys).

tableL(Vars, Goal) :- bindList(Vars), toString(Goal, Res), intersp(Vars, ' ', Lst), writeLst(Lst), writeLst([' => ', Res]), nl, fail.
tableL(_, _).

vars(term):
