is_prime(N, M) :- M > sqrt(N).
is_prime(N, M) :-
        N > M,
        Rem is N mod M,
        Rem \= 0,
        is_prime(N, M+1).

is_prime(N) :- is_prime(N, 2).
