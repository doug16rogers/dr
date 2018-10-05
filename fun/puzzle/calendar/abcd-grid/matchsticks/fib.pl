
fib_db(0, 0).
fib_db(1, 1).

fib(N,F) :- fib_db(N, F), !.
fib(N,F) :-
    Nm2 is N - 2,
    fib(Nm2, Fm2),
    Nm1 is N - 1,
    fib(Nm1, Fm1),
%    F is Fm2 + Fm1.
    F is Fm2 + Fm1,
    asserta(fib_db(N, F)).

