cg_preset(G) :- G is [[false, false, false,    25, false],
                      [false,     0, false, false, false],    %% The 0 is in the wrong place!
                      [    5, false,     5, false, false],
                      [false, false, false,     5, false],
                      [   20, false, false,    25,     5]].
cg_row_sum(S) :- S is [60,50,35,55,60].
cg_col_sum(S) :- S is [30,65,50,80,35].
cg_dia_sum(S) :- S is [30,60].

coin_valid(0).
coin_valid(5).
coin_valid(10).
coin_valid(25).

foldl(_, Vn, [], Vn).
foldl(BinaryOperation, V0, [Head | Tail], Vn) :-
    call(BinaryOperation, V0, Head, NextV),
    foldl(BinaryOperation, NextV, Tail, Vn).

add(X,Y,S) :- S is X + Y.
list_sum(L, Sum) :- foldl(add, 0, L, Sum).

row_sum_correct(R, Sum) :- list_sum(R, RSum), RSum is Sum.

rows_are_correct([], []).
rows_are_correct([Row | RowTail], [RowSum | RowSumTail]) :-
    row_sum_correct(Row, RowSum),
    rows_are_correct(RowTail, RowSumTail).

% Add: cols_are_correct(), diags_are_correct(), etc.

row_has_valid_coins([]).
row_has_valid_coins([Head | Tail]) :- coin_valid(Head), row_has_valid_coins(Tail).

grid_has_valid_coins([]).
grid_has_valid_coins([Row | Rest]) :-
    row_has_valid_coins(Row),
    grid_has_valid_coins(Rest).
