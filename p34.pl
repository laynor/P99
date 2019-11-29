:- ensure_loaded(p33).
phix(M, N, 1) :- coprime(M, N), !.
phix(_, _, 0).

% phi(M, N, Phi) where
% Phi is the number of integers R between 1 and N that are coprime with M.
phi2(_, 1, 1).
phi2(M, N, Phi) :-
         N>1, N1 is N-1,
         phi2(M, N1, Phi1),
         phix(M, N, K),
         Phi is Phi1 + K.

phi2(M, Phi) :- phi2(M, M, Phi).
