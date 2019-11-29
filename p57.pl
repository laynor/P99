:- ensure_loaded(p56).
add(X,nil,t(X,nil,nil)).
add(X,t(Root,L,R),t(Root,L1,R)) :- X @< Root, add(X,L,L1).
add(X,t(Root,L,R),t(Root,L,R1)) :- X @> Root, add(X,R,R1).

construct(Xs, T) :- reverse(Xs, R), construct1(R, T).
construct1([], nil).
construct1([X|Xs], T) :- construct1(Xs, T1), add(X, T1, T).

test_symmetric(L) :- construct(L, T), symmetric(T).