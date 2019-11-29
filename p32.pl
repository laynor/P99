:- use_package(fsyntax).
:- use_package(assertions).
:- use_package(nativeprops).
% :- use_package(clpqf).

:- fun_eval arith(true).
:- fun_eval defined(true).

:- pred mygcd(+int, +int, -int) + is_det.
:- fun_eval mygcd/2.

mygcd(A, 0, A).
mygcd(A, B, C) :- A\=B, B\=0,
        B1 is A mod B,
        mygcd(B, B1, C).

foo(X) :- X is mygcd(9, 6).
