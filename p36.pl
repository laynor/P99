pfm(N, Fs) :- pfm(N, 2, 0, Fs).
% pfm(N, F, K, Fs)
% Fs is the list of factors of N.
pfm(1, _, 0, []).                               % Empty list, K = 0 iff computations started with N = 1.
pfm(1, F, M, [[F, M]]) :- M > 0.                % Last step.

pfm(N, F, M, Fs) :-
        N mod F =:= 0, !,                       % it's a factor.
        N1 is N // F,
        M1 is M+1,
        pfm(N1, F, M1, Fs).

pfm(N, M, 0, Fs) :- M1 is M+1, pfm(N, M1, 0, Fs).
pfm(N, M, K, [[M, K]|Fs]) :- K>0, M1 is M+1, pfm(N, M1, 0, Fs).

% --------------- Other solution

divide(M, N, Pow, Q) :- divide(M, N, 0, Pow, Q).
% divide(M, N, C, Pow, Q)
% M * N^C = Q * N^Pow
divide(M, N, C, C, M) :- M mod N =\= 0, !.
divide(M, N, C, Pow, Q) :- M1 is M // N, C1 is C+1, divide(M1, N, C1, Pow, Q).

nextfactor(2, 3).
nextfactor(F, NF) :- F>2, NF is F + 2.

pfm2(1, _, []).
pfm2(N, M, [N]) :- M > sqrt(N).
pfm2(N, F, [[F, M]|Fs]) :- divide(N, F, M, N1), M > 0, !, nextfactor(F, NF), pfm2(N1, NF, Fs).
pfm2(N, F, Fs) :- nextfactor(F, NF), pfm2(N, NF, Fs).
pfm2(N, Fs) :- pfm2(N, 2, Fs).