and(A, B)  :- A,B.
or(A, _)   :- A.
or(_, B)   :- B.
not(A)     :- \+ A.
nand(A, B) :- not(and(A, B)).
nor(A, B)  :- not(or(A, B)).
equ(A, B)  :- or(and(A, B), and(not(A), not(B))).
xor(A, B)  :- not(equ(A, B)).
impl(A, B) :- or(not(A), B).

writeLst(L) :- member(X, L), padtrue(X, Y), write(Y), fail.
writeLst(_).

toString(Goal, true ) :- Goal, !.
toString(_, false).

padtrue(true, 'true ') :- !.
padtrue(X, X).

bind(false).
bind(true).

table(A, B, Goal) :- bind(A), bind(B), toString(Goal, Res), writeLst([A, ' ', B, ' => ', Res]), nl, fail.
table(_, _, _).
