:- ensure_loaded(p20).
insert_at(Xs, X, K, Ys) :- removek(Ys, X, K, Xs), !.
