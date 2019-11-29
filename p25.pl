:- ensure_loaded(p23).
perm(L, P) :- length(L, N), rnd_select(L, N, P).
