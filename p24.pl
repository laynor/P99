:- ensure_loaded(p22).
:- ensure_loaded(p23).
lotto(M, N, L) :- range(1, M, Ns), rnd_select(Ns, N, L).
