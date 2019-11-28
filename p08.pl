remdup([], []).
remdup([X],[X]).
remdup([X,X|Xs], Ys) :- remdup([X|Xs], Ys), !.
remdup([X,Y|Xs], [X|Ys]) :- remdup([Y|Xs], Ys). % No need to check X /= Y because of the cut operator above O_o
