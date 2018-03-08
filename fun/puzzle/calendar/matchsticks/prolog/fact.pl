
fact_db(0,1).

fact(N,F) :- fact_db(N,F), !.
fact(N,F) :-
    Nm1 is N - 1,
    fact(Nm1, Fm1),
    %% F is N * Fm1.
    F = N * Fm1,
    asserta(fact_db(N, F)).
