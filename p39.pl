:- ensure_loaded(p31).
:- ensure_loaded

nextfactor(2, 3).
nextfactor(F, NF) :- F>2, NF is F + 2.

primes(N, M, Fs) :- N < 2, !, primes(2, M, Fs).
primes(N, M, [M]) :- N >= M, is_prime(M), !.
primes(N, M, []) :- N >= M.
primes(N, M, [N|Ps]) :- is_prime(N), nextfactor(N, N1), primes(N1, M, Ps).
primes(N, M, Ps) :- nextfactor(N, N1), primes(N1, M, Ps).
