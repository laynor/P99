% :- module(p99,
%         [rnd_select/3, split/4, lotto/3,
%          perm/2, comb/3, remove/3, group3/4,
%          combr/4, group/3, msort/2, lsort/2, merge/3,
%          lfsort/2, mksort/2, is_prime/1, mygcd/3,
%          phi/2, phix/3, range/3, factors/2, pfm/2, pfm2/2, divide/3, divide/4]
%         ).

:- use_module(library(lists), [append/3] ).
:- use_module(library(random), [random/3, random/1]).

:- use_package(fsyntax).
:- use_package(assertions).
:- use_package(nativeprops).
% :- use_package(clpqf).

:- fun_eval arith(true).
:- fun_eval defined(true).


% my_last([]) :- false.
% element_at(X, L, N)

% rev(L, R).
% rev([], []).
% rev([X|Xs], Y) :- rev(Xs, Y1), append(Y1, [X], Y).


% flatten(Xs, Ys)


rle1([], []).
rle1([X], [[1,X]]).
rle1([X, X|Ys], [[N, X]|Zs]) :- rle1([X|Ys], [[N1, X]|Zs]), N is N1 + 1, !.
rle1([X, Y|Xs], [[1, X]|Zs]) :- rle1([Y|Xs], Zs).
rledir(L1, Enc) :- rle1(L1, Enc1), rleComp(Enc1, Enc).


dup([],[]).
dup([X|Xs], [X, X|Ys]) :- dup(Xs,Ys).

mylistn(0, _, []).
mylistn(N, X, [X|Xs]) :- N>0, N1 is N - 1, mylistn(N1, X, Xs).

mydupn(_, [], []).
mydupn(N, [X|Xs], Ys) :- mylistn(N, X, NX), mydupn(N, Xs, NXs), append(NX, NXs, Ys).

% drop(N, L, R)
% _drop(N, N, L, R) :-
mydroph(_, _, [], []).
mydroph(1, N, [_|Xs], Ys) :- mydroph(N, N, Xs, Ys),!.
mydroph(M, N, [X|Xs], [X|Ys]) :- M1 is M-1, mydroph(M1, N, Xs, Ys).
mydrop(N, L, R) :- mydroph(N, N, L, R).

% mysplit(L, N, L1, L2)
split(L, 0, [], L).
split([X|Xs], N, [X|L1], L2) :- N1 is N-1, split(Xs, N1, L1, L2).

split2(Xs, N, L, R) :- length(L, N), append(L, R, Xs).

slice(Xs, Beg, End, Slice) :- B is Beg-1, split(Xs, End, L, _), split(L, B, _, Slice).

rotln(Xs, N, Ys) :- N>=0, split(Xs, N, L, R), append(R, L, Ys).
rotln(Xs, N, Ys) :- N<0, length(Xs, L), N1 is L+N, rotln(Xs, N1, Ys).

removek(Xs, X, K, Ys) :- K1 is K-1, split(Xs, K1, L, [X|R]), append(L, R, Ys).

% insert_at(X, Xs, N, Ys) :- N1 is N-1, split(Xs, N1, L, R), append(L, [X|R], Ys).
insert_at(Xs, X, K, Ys) :- removek(Ys, X, K, Xs), !.

range(N, M, []) :- N > M.
range(N, M, [N|Xs]) :- N=<M, N1 is N+1, range(N1, M, Xs).


rnd_select([], _, []).
rnd_select(_, 0, []).
rnd_select(L, N, [X|Xs]) :-
        N > 0,
        length(L, Len),
        random(1, Len, I1),
        removek(L, X, I1, L1),
        N1 is N-1,
        rnd_select(L1, N1, Xs).

lotto(M, N, L) :- range(1, M, Ns), rnd_select(Ns, N, L).

perm(L, P) :- length(L, N), rnd_select(L, N, P).

% comb(N, L, K)
% comb(N, L, [X|C]) :-
%         N1 is N-1,
%         length(L, Len),
%         range(1, Len, Ks),
%         member(K,Ks),
%         removek(L, X, K, L1),
%         comb(N1, L1, C).

remove(_, [], []).
remove(X, [X|Xs], Xs) :- !.
remove(X, [Y|Xs], [Y|Ys]) :- remove(X, Xs, Ys).


el(X, [X|R], R).
el(X, [_|L], R) :- el(X, L, R).

comb(0, _, []).
comb(N, L, [X|C]) :-
        el(X, L, R),
        N1 is N-1,
        comb(N1, R, C).

elLR(X, Xs, L, R) :- el(X, Xs, R), append(L, [X|R], Xs).

combr(0, Rc, [], Rc).
combr(N, Xs, [X|C], Rc) :-
        elLR(X, Xs, L, R),
        N1 is N-1,
        combr(N1, R, C, Rc1),
        append(L, Rc1, Rc).

group3(Xs, G1, G2, G3) :-
        combr(2, Xs, G1, R1),
        combr(3, R1, G2, R2),
        combr(4, R2, G3, []).

group(_, [], []).
group(Xs, [N|Ns], [G|Gs]) :-
        combr(N, Xs, G, R),
        group(R, Ns, Gs).

% P28 -- sorting

msort([], []) :- !.
msort([X], [X]) :- !.
msort(Xs, Sorted) :-
        length(Xs, Len),
        Len > 1,
        HalfLen is ceiling(Len/2),
        split(Xs, HalfLen, Left, Right),
        msort(Left, LeftSorted),
        msort(Right, RightSorted),
        merge(LeftSorted, RightSorted, Sorted).


merge([], Ys, Ys) :- !.
merge(Xs, [], Xs) :- !.
merge([X|Xs], [Y|Ys], [X|Zs]) :-
        X =< Y,
        merge(Xs, [Y|Ys], Zs).

merge([X|Xs], [Y|Ys], [Y|Zs]) :-
        X > Y,
        merge([X|Xs], Ys, Zs).

lsort([], []) :- !.
lsort([X], [X]) :- !.
lsort(Xs, Sorted) :-
        length(Xs, Len),
        Len > 1,
        HalfLen is ceiling(Len/2),
        split(Xs, HalfLen, Left, Right),
        lsort(Left, LeftSorted),
        lsort(Right, RightSorted),
        lmerge(LeftSorted, RightSorted, Sorted).

lmerge([], Ys, Ys) :- !.
lmerge(Xs, [], Xs) :- !.
lmerge([X|Xs], [Y|Ys], [X|Zs]) :-
        length(X, Lx),
        length(Y, Ly),
        Lx =< Ly,
        lmerge(Xs, [Y|Ys], Zs).

lmerge([X|Xs], [Y|Ys], [Y|Zs]) :-
        length(X, Lx),
        length(Y, Ly),
        Lx > Ly,
        lmerge([X|Xs], Ys, Zs).



mergek([], Ys, Ys) :- !.
mergek(Xs, [], Xs) :- !.
mergek([X|Xs], [Y|Ys], [X|Zs]) :-
        X = [Kx, _],
        Y = [Ky, _],
        Kx =< Ky,
        mergek(Xs, [Y|Ys], Zs).

mergek([X|Xs], [Y|Ys], [Y|Zs]) :-
        X = [Kx, _],
        Y = [Ky, _],
        Kx > Ky,
        mergek([X|Xs], Ys, Zs).

mksort([], []) :- !.
mksort([X], [X]) :- !.
mksort(Xs, Sorted) :-
        length(Xs, Len),
        Len > 1,
        HalfLen is ceiling(Len/2),
        split(Xs, HalfLen, Left, Right),
        mksort(Left, LeftSorted),
        mksort(Right, RightSorted),
        mergek(LeftSorted, RightSorted, Sorted).



lens([], []).
lens([X|Xs], [L|Ls]) :- length(X, L), lens(Xs, Ls).

zip([], [], []).
zip([X|Xs], [Y|Ys], [[X,Y]|Zs]) :- zip(Xs, Ys, Zs).

withLens(Xs, Ys) :- lens(Xs, Ls), zip(Ls, Xs, Ys).

groupk([], nil, _, []).
groupk([], K, Vs, [[K, Vs]]).
groupk([[K, V]|KVs], K, Vs, Gs) :- groupk(KVs, K, [V|Vs], Gs).
groupk([[K1, V]|KVs], K2, Vs, [[K2, Vs]|Gs]) :- K1 \= K2, groupk([[K1, V]|KVs], K1, [], Gs).

values([], []).
values([[_,V]|KVs], [V|Vs]) :- values(KVs, Vs).

concat([], []).
concat([X|Xs], Ys) :- concat(Xs, Zs), append(X, Zs, Ys).

lfsort(Xs, Ys) :-
        withLens(Xs, LXs),
        mksort(LXs, [[L, X]|SLXs]),
        groupk(SLXs, L, [X], Gs),
        values(Gs, ByLen),
        withLens(ByLen, FreqTagged),
        mksort(FreqTagged, FreqSorted),
        values(FreqSorted, Zs),
        concat(Zs, Ys).

is_prime(N, M) :- M > sqrt(N).
is_prime(N, M) :-
        N > M,
        Rem is N mod M,
        Rem \= 0,
        is_prime(N, M+1).

is_prime(N) :- is_prime(N, 2).

:- pred mygcd(+int, +int, -int) + is_det.
:- fun_eval mygcd/2.

mygcd(A, 0, A).
mygcd(A, B, C) :- A\=B, B\=0,
        B1 is A mod B,
        mygcd(B, B1, C).

foo(X) :- X is mygcd(9, 6).

% gcd1(A, 0) := A.
% gcd1(A, B) :=
%         A\=B,
%         B\=0,
%         B1 is A mod B,
%         gcd1(B, B1).

coprime(X, Y) :- mygcd(X, Y, 1).


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

% phi(M, Phi)

% Many conditions here can be avoided using the ! predicate.
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

% M = Q * N^Pow
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

phiterm(P, M, T) :- T is (P - 1) * (P ** (M - 1)).

phih([], 1).
phih([[P, M]|Fs], Phi) :- phiterm(P, M, T), phih(Fs, Phi1), Phi is Phi1 * T.

phi(M, Phi) :-
        pfm2(M, Fs),
        phih(Fs, Phi).

% primes(X, Y, Fs)
primes(N, M, Fs) :- N < 2, !, primes(2, M, Fs).
primes(N, M, [M]) :- N >= M, is_prime(M), !.
primes(N, M, []) :- N >= M.
primes(N, M, [N|Ps]) :- is_prime(N), nextfactor(N, N1), primes(N1, M, Ps).
primes(N, M, Ps) :- nextfactor(N, N1), primes(N1, M, Ps).

goldbach(N, L) :- primes(2, N, Ps), member(A, Ps), member(B, Ps), N is A + B, L = [A, B].


writeLst(L) :- member(X, L), write(X), fail.
writeLst(_).

goldbach_list(N, M, Lim) :- 1 is N mod 2, N1 is N+1, goldbach_list(N1, M, Lim).
goldbach_list(N, M, Lim) :-
        N =< M,
        goldbach(N, [P1, P2]),
        (P1 > Lim, writeLst([N, ' = ', P1, ' + ', P2]), nl ; true),
        N1 is N+2, goldbach_list(N1, M, Lim).
goldbach_list(_, _, _).
