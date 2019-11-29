:- ensure_loaded(p17).
slice(Xs, Beg, End, Slice) :- B is Beg-1, split(Xs, End, L, _), split(L, B, _, Slice).
