%% Use: GLOBALSZ=850000 LOCALSZ=524288 TRAILSZ=524288 gprolog --consult-file cal_20180911.pl

%%  HOLES
%%  
%%  Locate 12 holes in the empty cells of
%%  this grid. The numbers outside the
%%  grid indicate the number of holes in
%%  each row or column. Each arrow in the
%%  grid points directly toward one or more
%%  of the holes. An arrow may be immediately
%%  next to the hole it points to, or all the way
%%  across the grid. Not every hole will
%%  necessarily have an arrow pointing to it.
%%  Can you locate the 12 holes?
%%  
%%         3   1   1   1   1   2   1   2
%%       +---+---+---+---+---+---+---+---+
%%    2  |   |   |<- |   |   |   |   |   |
%%       +---+---+---+---+---+---+---+---+
%%    2  |   |   |   |   |   |   |   |   |
%%       +---+---+---+---+---+---+---+---+
%%    2  |   |   |<- | v |   | ->|   |   |
%%       +---+---+---+---+---+---+---+---+
%%    1  | \,| /'|   |   |   |   |   |   |
%%       +---+---+---+---+---+---+---+---+
%%    2  |   |   | ->|   |   | v |'\ |   |
%%       +---+---+---+---+---+---+---+---+
%%    1  |   |   |   |   |   |   |'\ |   |
%%       +---+---+---+---+---+---+---+---+
%%    1  | /'|   |   |   |   |   |   |   |
%%       +---+---+---+---+---+---+---+---+
%%    1  | /'|   |   |   |   |   |   |   |
%%       +---+---+---+---+---+---+---+---+

%%  Solution:
%%         3   1   1   1   1   2   1   2
%%       +---+---+---+---+---+---+---+---+
%%    2  | O |   |<- |   | O |   |   |   |
%%       +---+---+---+---+---+---+---+---+
%%    2  |   |   | O |   |   |   | O |   |
%%       +---+---+---+---+---+---+---+---+
%%    2  | O |   |<- | v |   | ->|   | O |
%%       +---+---+---+---+---+---+---+---+
%%    1  | \,| /'|   |   |   | O |   |   |
%%       +---+---+---+---+---+---+---+---+
%%    2  | O |   | ->|   |   | v |'\ | O |
%%       +---+---+---+---+---+---+---+---+
%%    1  |   | O |   |   |   |   |'\ |   |
%%       +---+---+---+---+---+---+---+---+
%%    1  | /'|   |   | O |   |   |   |   |
%%       +---+---+---+---+---+---+---+---+
%%    1  | /'|   |   |   |   | O |   |   |
%%       +---+---+---+---+---+---+---+---+

%% Arrow directions taken from phone (not computer) keypad:
%%
%%  +---+---+---+
%%  | 1 | 2 | 3 |   1=NW  2=N  3=NE
%%  +---+---+---+       \  |  /
%%  | 4 | 5 | 6 |   4=W - (5) - 6=E
%%  +---+---+---+       /  |  \
%%  | 7 | 8 | 9 |   7=SW  8=S  8=SE
%%  +---+---+---+
%%  | * | 0 | # |
%%  +---+---+---+
%%
holes_arrow_dir([[_,_,4,_,_,_,_,_],
                 [_,_,_,_,_,_,_,_],
                 [_,_,4,8,_,6,_,_],
                 [9,3,_,_,_,_,_,_],
                 [_,_,6,_,_,8,1,_],
                 [_,_,_,_,_,_,1,_],
                 [3,_,_,_,_,_,_,_],
                 [3,_,_,_,_,_,_,_]]).
holes_per_row([2,2,2,1,2,1,1,1]).
holes_per_col([3,1,1,1,1,2,1,2]).

row_count(RCount) :- holes_per_row(Rows), RCount is len(Rows).
col_count(CCount) :- holes_per_row(Cols), CCount is len(Cols).
row_okay(R) :- R >= 1, row_count(RCount), R =< RCount.
col_okay(C) :- C >= 1, row_count(CCount), C =< CCount.
row_col_okay(R, C) :- row_okay(R), col_okay(C).

arrow_cell(R, C, Cell) :-
    row_col_okay(R, C),
    holes_arrow_dir(ArrowGrid),
    nth(R, ArrowGrid, Row),
    nth(C, Row, Cell).

hole_cell(Holes, R, C, Cell) :-
    row_col_okay(R, C),
    nth(R, Holes, Row),
    nth(C, Row, Cell).

is_hole(Holes, R, C) :-
    row_col_okay(R, C),
    hole_cell(Holes, R, C, Cell),
    Cell == 'O'.

hole_in(Holes, R, C, _, _) :- is_hole(Holes, R, C).
hole_in(Holes, R, C, IncR, IncC) :-
    NextR is R + IncR,
    NextC is C + IncC,
    hole_in(Holes, NextR, NextC, IncC).

hole_in_line(Holes, StartR, StartC, IncR, IncC) :-
    R is StartR + IncR,
    C is StartC + IncC,
    hole_in(Holes, R, C, IncR, IncC),
    
arrow_cell_satisfied(Holes, R, C, _).
arrow_cell_satisfied(Holes, R, C, 1) :- hole_in_line(Holes, R, C, -1, -1).
arrow_cell_satisfied(Holes, R, C, 2) :- hole_in_line(Holes, R, C, -1,  0).
arrow_cell_satisfied(Holes, R, C, 3) :- hole_in_line(Holes, R, C, -1, +1).
arrow_cell_satisfied(Holes, R, C, 4) :- hole_in_line(Holes, R, C,  0, -1).
arrow_cell_satisfied(Holes, R, C, 6) :- hole_in_line(Holes, R, C,  0, +1).
arrow_cell_satisfied(Holes, R, C, 7) :- hole_in_line(Holes, R, C, +1, -1).
arrow_cell_satisfied(Holes, R, C, 8) :- hole_in_line(Holes, R, C, +1,  0).
arrow_cell_satisfied(Holes, R, C, 9) :- hole_in_line(Holes, R, C, +1, +1).

arrow_satisfied(Holes, R, C) :- arrow_cell(R, C, Cell), arrow_cell_satisfied(Holes, R, C, Cell).

arrow_col_satisfied(Holes, R, C) :- col_okay(C), arrow_satisfied(Holes, R, C).
arrow_col_satisfied(Holes, R, C) :- NextC is C + 1, arrow_col_satisfied(Holes, R, NextC).

arrow_row_satisfied(Holes, R) :- row_okay(R), arrow_col_satisfied(Holes, R, 1).
arrow_row_satisfied(Holes, R) :- NextR is R + 1, arrow_row_satisfied(Holes, NextR).

arrows_satisfied(Holes) :-
    arrow_row_satisfied(Holes, 1).

holes_okay(Holes) :-
    arrows_satisfied(Holes).

