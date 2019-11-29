:- ensure_loaded(p55).
:- ensure_loaded(p56).

sym_cbalt(N, Ts) :- findall(T, (cbalt(N, T), symmetric(T)), Ts).

symn(N, M) :- sym_cbalt(N, Ts), length(Ts, M).