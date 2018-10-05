%% Solver for ABCD grid puzzles from Mensa's Puzzle-A-Day Calendar.

%%                                              +---+---+---+---+---+
%%   Place one of A, B, C, or                 A | 0 | 2 | 3 | 0 | 1 |
%%   D into each of the 25                       +---+---+---+---+---+
%%   empty cells so that the                  B | 2 | 0 | 1 | 2 | 2 |
%%   number of letters in each                  +---+---+---+---+---+
%%   row and column is as                     C | 1 | 1 | 0 | 2 | 1 |
%%   indicated by the numbers.                  +---+---+---+---+---+
%%   Identical letters cannot     A   B   C   D | 2 | 2 | 1 | 1 | 1 |
%%   be next to each other      +---+---+---+---+---+---+---+---+---+
%%   horizontally or            | 1 | 1 | 1 | 2 |   |   |   |   |   |
%%   vertically.                +---+---+---+---+---+---+---+---+---+
%%                              | 1 | 3 | 1 | 0 |   |   |   |   |   |
%%                              +---+---+---+---+---+---+---+---+---+
%%                              | 1 | 0 | 2 | 2 |   |   |   |   |   |
%%                              +---+---+---+---+---+---+---+---+---+
%%                              | 2 | 1 | 0 | 2 |   |   |   |   |   |
%%                              +---+---+---+---+---+---+---+---+---+
%%                              | 1 | 2 | 1 | 1 |   |   |   |   |   |
%%                              +---+---+---+---+---+---+---+---+---+
%%
%% Solutions use A=0, B=1, C=2, D=3, ...
%% legal_board([[3,2,0,1,3],[1,0,1,2,1],[2,3,0,3,2],[3,0,3,1,0],[1,3,0,2,1]]).

abcd_col_counts([[0,2,1,2],[2,0,1,2],[3,1,0,1],[0,2,2,1],[1,2,1,1]]).
abcd_row_counts([[1,1,1,2],[1,3,1,0],[1,0,2,2],[2,1,0,2],[1,2,1,1]]).
%% abcd_value_count(4).
abcd_value_count(Count) :-
    abcd_row_counts([Row | _ ]),
    list_length(Row, Count).

%% Test - won't work with adjacency constraint:
%%    
%%           +---+---+
%%         A | 0 | 1 |
%%            +---+---+
%%     A   B | 2 | 1 |
%%  -+---+---+---+---+
%%   | 0 | 2 | b | b |
%%  -+---+---+---+---+
%%   | 1 | 1 | b | a |
%%  -+---+---+---+---+
%%
%% Solution should be: [[1,1],[1,0]].

%% abcd_col_counts([[0,2],[1,1]]).   %% won't work with adjacency
%% abcd_row_counts([[0,2],[1,1]]).   %% won't work with adjacency

%% Utility:
write_lists([]).
write_lists([Head | Tail]) :- write(Head), nl, write_lists(Tail).

%% Row-major.
legal_board(B) :-
    abcd_row_counts(CheckRows),
    abcd_col_counts(CheckCols),
    abcd_value_count(ValueCount),
    list_length(CheckRows, GridRows),
    list_length(CheckCols, GridCols),
    %% Rows == Cols,             %% Shouldn't be required.
    legal_board_row_counts(CheckRows, 0, ValueCount, B),
    transpose(B, BT),
    legal_board_row_counts(CheckCols, 0, ValueCount, BT),
    legal_board_row_adjacency(B, 0, GridRows),
    legal_board_row_adjacency(BT, 0, GridCols).

legal_board_row_counts([], _, []).
legal_board_row_counts([CheckRow | CheckRest], Value, ValueCount, [Row | Rest]) :-
    %% Value < ValueCount,
    legal_row_counts(Value, CheckRow, Row),
    NextValue is Value + 1,
    legal_board_row_counts(CheckRest, NextValue, ValueCount, Rest).

legal_row_counts(_, [], _).
legal_row_counts(Val, [CheckCount | CheckRest], Row) :-
    count_occurrences_in_list(Row, Val, Count),
    Count == CheckCount,
    Vp1 is Val + 1,
    legal_row_counts(Vp1, CheckRest, Row).

count_occurrences_in_list([], _, 0).
count_occurrences_in_list([Val | Rest], Val, Count) :-
    count_occurrences_in_list(Rest, Val, RestCount),
    Count is RestCount + 1.
count_occurrences_in_list([NotVal | Rest], Val, Count) :-
    NotVal \== Val,
    count_occurrences_in_list(Rest, Val, Count).

legal_board_row_adjacency([], _, _).
legal_board_row_adjacency([Row | Rest], N, Max) :-
    N < Max,
    legal_row_adjacency(Row),
    Np1 is N + 1,
    legal_board_row_adjacency(Rest, Np1, Max).

legal_row_adjacency([_ | []]).
legal_row_adjacency([X | [Y | Rest]]) :-
    X \== Y,
    legal_row_adjacency([Y | Rest]).

list_length([], 0).
list_length([_ | Tail], Length) :-
    list_length(Tail, Sub),
    Length is Sub + 1.

%% From https://stackoverflow.com/questions/4280986/how-to-transpose-a-matrix-in-prolog?rq=1.
transpose([], []).
transpose([F|Fs], Ts) :-
    transpose(F, [F|Fs], Ts).

transpose([], _, []).
transpose([_|Rs], Ms, [Ts|Tss]) :-
    lists_firsts_rests(Ms, Ts, Ms1),
    transpose(Rs, Ms1, Tss).

lists_firsts_rests([], [], []).
lists_firsts_rests([[F|Os]|Rest], [F|Fs], [Os|Oss]) :-
    lists_firsts_rests(Rest, Fs, Oss).

