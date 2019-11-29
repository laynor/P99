factors(N, Factors) :- factors(N, 2, Factors).
factors(N, M, []) :- N<M.
factors(N, N, [N]).
factors(N, M, [M|Fs]) :-
        M<N, 0 =:= N mod M,
        N1 is N // M,
        factors(N1, M, Fs).
factors(N, M, Fs) :-
        M<N, 0 =\= N mod M,
        M1 is M+1,
        factors(N, M1, Fs).
