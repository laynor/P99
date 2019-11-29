:- ensure_loaded(p39).
goldbach(N, L) :- primes(2, N, Ps), member(A, Ps), member(B, Ps), N is A + B, L = [A, B].
