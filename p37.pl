:- ensure_loaded(p36).
phiterm(P, M, T) :- T is (P - 1) * (P ** (M - 1)).

phih([], 1).
phih([[P, M]|Fs], Phi) :- phiterm(P, M, T), phih(Fs, Phi1), Phi is Phi1 * T.

phi(M, Phi) :-
        pfm2(M, Fs),
        phih(Fs, Phi).
