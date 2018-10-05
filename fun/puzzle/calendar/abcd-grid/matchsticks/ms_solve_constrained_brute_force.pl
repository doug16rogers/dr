% -----------------------------------------------------------------------------
% This really only works for 2D grids. It assumes a leading character
% (typically 'm') followed by a row number or column letter, then two of the
% opposite.
coords_list(Row, Col, Row, Col, _,  _, [[Row, Col]]).
coords_list(HRow, HCol, TRow, TCol, RowStep, ColStep, [[HRow, HCol] | CoordsTail]) :-
    ((HRow \== TRow) ; (HCol \== TCol)),
    NextRow is HRow + RowStep,
    NextCol is HCol + ColStep,
    coords_list(NextRow, NextCol, TRow, TCol, RowStep, ColStep, CoordsTail).
base_to_num(Char, BaseChar, Num) :- char_code(BaseChar, Base), char_code(Char, Code), Num is 1 + Code - Base.
char_to_row(Char, Row) :- ('0' == Char) -> Row is 10; base_to_num(Char, '1', Row).
char_to_col(Char, Col) :- base_to_num(Char, 'a', Col).
char_to_coords(HRowChar, HColChar, TRowChar, TColChar, RowStep, ColStep, Coords) :-
    char_to_row(HRowChar, HRow), char_to_col(HColChar, HCol),
    char_to_row(TRowChar, TRow), char_to_col(TColChar, TCol),
    coords_list(HRow, HCol, TRow, TCol, RowStep, ColStep, Coords).
char_between(LoChar,HiChar,Char) :-
    char_code(LoChar, Lo), char_code(HiChar, Hi), char_code(Char, Code),
    (Lo =< Code), (Code =< Hi).
char_to_coords(LeadChar, HeadChar, TailChar, Step, Coords) :-
    char_between('0', '9', LeadChar),
    char_to_coords(LeadChar, HeadChar, LeadChar, TailChar, 0, Step, Coords).
char_to_coords(LeadChar, HeadChar, TailChar, Step, Coords) :-
    char_between('a', 'z', LeadChar),
    char_to_coords(HeadChar, LeadChar, TailChar, LeadChar, Step, 0, Coords).
direction_step('<', 1).
direction_step('>', -1).
name_to_coords(Name, Coords) :-
    atom_chars(Name, [NameChar | [LeadChar | [HeadChar | [TailChar]]]]),
    NameChar == 'm',
    compare(Direction, HeadChar, TailChar),
    direction_step(Direction,Step),
    char_to_coords(LeadChar, HeadChar, TailChar, Step, Coords).

ms_coords_from_list([], []).
ms_coords_from_list([MsHead | MsTail], [CoordHead | CoordTail]) :-
    name_to_coords(MsHead, CoordHead),
    ms_coords_from_list(MsTail, CoordTail).

% Matchstick list with each matchstick represented as a list of 1-based
% coordinates starting at the match head (the part that burns first).
ms(Ms) :-
    ms_names(MsListByName),
    ms_coords_from_list(MsListByName, Ms).

% -----------------------------------------------------------------------------
% These are predicates for use in checking that a pattern is valid:
ms_dimension(D) :- ms_grid_size(Sizes), length(Sizes, D).
foldl(_, Vn, [], Vn).
foldl(BinaryOperation, V0, [Head | Tail], Vn) :-
    call(BinaryOperation, V0, Head, NextV),
    foldl(BinaryOperation, NextV, Tail, Vn).
add(X,Y,S) :- S is X + Y.
mul(X,Y,P) :- P is X * Y.
list_product(L, Product) :- foldl(mul, 1, L, Product).
list_sum(L, Sum) :- foldl(add, 0, L, Sum).
ms_cell_count(CellCount) :-
    ms_grid_size(Sizes),
    list_product(Sizes, 1, CellCount).

add_each_in_list_of_lists([], Out, Out).
add_each_in_list_of_lists([HL | TL], Acc, Out) :-
    append(HL, Acc, HOut),
    add_each_in_list_of_lists(TL, HOut, Out).
ms_all_cells(CellList) :-
    ms(MatchStickList),
    add_each_in_list_of_lists(MatchStickList, [], CellList).

%list_covers_range(L, Coord, MaxCoord) :-
%    MaxCoord is [].

list_range(_, Last, K, []) :- K > Last.
list_range(First, Last, K, [K | Tail]) :-
    K =< Last,
    NextK is K + 1,
    list_range(First, Last, NextK, Tail).
list_range(Last, Last, [Last]).
list_range(First, Last, L) :- list_range(First, Last, First, L).

make_list_of_lists([], []).
make_list_of_lists([Head | Tail], [[Head] | OutTail]) :-
    make_list_of_lists(Tail, OutTail).
    
span(_, [], []).
span(N, [HeadList | TailLists], [ [N | HeadList] | TailListOut ]) :-
    span(N, TailLists, TailListOut).

spanlist([], _, []).
spanlist([Head | Tail], [], [[Head] | SpanListTail]) :-   % Or: make_list_of_lists([Head|Tail], Out).
    spanlist(Tail, [], SpanListTail).
spanlist([Head | Tail], L2, Out) :-
    L2 \= [],
    span(Head, L2, SpanOut),
    spanlist(Tail, L2, SpanListOut),
    append(SpanOut, SpanListOut, Out).

outer_product([], []).
outer_product([HeadList | TailLists], Out) :-
    outer_product(TailLists, TailOuterProduct),
    spanlist(HeadList, TailOuterProduct, Out).

list_range1to(N, L) :- list_range(1, N, L).

list_all_coords(Sizes, AllCoords) :-
    maplist(list_range1to, Sizes, SizeRanges),
    outer_product(SizeRanges, AllCoords).

% Built-in:
% member(Item, [Item|_]).
% member(Item, [_|Tail]) :- member(Item, Tail).

list_contains_all([], _).
list_contains_all([Head | Tail], L) :-
    member(Head, L),
    list_contains_all(Tail, L).

list_lengths([], []).
list_lengths([Head | Tail], [LenHead | LenTail]) :-
    length(Head, LenHead),
    list_lengths(Tail, LenTail).

burn_sums_valid([], []).
burn_sums_valid([BurnSum | BurnSumTail], [Size | SizeTail]) :-
    length(BurnSum, BurnSumLength),
    BurnSumLength == Size,
    burn_sums_valid(BurnSumTail, SizeTail).
    % @todo: Check that burn sums are less than other dimensions - need to
    %        think through rectangles and n-dimensional problems.
        
ms_placement_valid :-
    ms_all_cells(CellList),
    ms_cell_count(CellCount),           % cell count taken from grid size.
    length(CellList, CellListLength),   % cell count taken from matchstick list.
    CellCount == CellListLength,
    ms_grid_size(Sizes),
    list_all_coords(Sizes, AllCoords),
    list_contains_all(AllCoords, CellList),
    ms_burn_sums(BurnSums),
    burn_sums_valid(BurnSums, Sizes).
    
zero_list_with_length(0, []).
zero_list_with_length(Len, [0 | ZeroTail]) :-
    Len > 0,
    TailLen is Len - 1,
    zero_list_with_length(TailLen, ZeroTail).

zero_list([], []).
zero_list([Head | Tail], [SumHead | SumTail]) :-
    zero_list_with_length(Head, SumHead),
    zero_list(Tail, SumTail).

replace_nth(0, L, _, L).
replace_nth(1, [_ | InTail], Item, [Item | InTail]).
replace_nth(N, [InHead | InTail], Item, [InHead | OutTail]) :-
    N > 1,
    NextN is N - 1,
    replace_nth(NextN, InTail, Item, OutTail).

% For each coordinate (axis), increment the counter at the burning index.
register_cell_burn([], Sum, Sum).
register_cell_burn([Coord | CoordTail], [SumInCoord | SumInCoordTail], [SumOutCoord | SumOutCoordTail]) :-
    nth(Coord, SumInCoord, SIn),
    SOut is SIn + 1,
    replace_nth(Coord, SumInCoord, SOut, SumOutCoord),
    register_cell_burn(CoordTail, SumInCoordTail, SumOutCoordTail).

register_burn(0, _, Sum, Sum).
register_burn(BurnCount, [MsCellHead | MsCellTail], SumIn, SumOut) :-
    BurnCount > 0,
    length(MsCellTail, TailLength),
    BurnCount =< (TailLength + 1),
    register_cell_burn(MsCellHead, SumIn, SumAfterCell),
    NextBurnCount is BurnCount - 1,
    register_burn(NextBurnCount, MsCellTail, SumAfterCell, SumOut).

burn_list_sums([], _, Sums, Sums).
burn_list_sums([BurnHead | BurnTail], [MsListHead | MsListTail], StartSums, Sums) :-
    BurnHead >= 0,
    length(MsListHead, MsLength),
    BurnHead =< MsLength,
    register_burn(BurnHead, MsListHead, StartSums, ThisSum),
    burn_list_sums(BurnTail, MsListTail, ThisSum, Sums).
    
burn_valid(Ms, MsGridSize, MsBurnSums, BurnList) :-
    length(BurnList, BurnLength),
    length(Ms, MsCount),
    BurnLength == MsCount,
    zero_list(MsGridSize, StartSums),
    burn_list_sums(BurnList, Ms, StartSums, BurnSums),
    BurnSums == MsBurnSums.

list_range_0toN(N, Ranges) :- list_range(0, N, Ranges).

check_burn_list_solutions(_, _, _, [], Solutions, Solutions).
check_burn_list_solutions(Ms, MsGridSize, MsBurnSums, [BurnList | BurnListsTail], Acc, Solutions) :-
    burn_valid(Ms, MsGridSize, MsBurnSums, BurnList) ->
        check_burn_list_solutions(Ms, MsGridSize, MsBurnSums, BurnListsTail, [BurnList | Acc], Solutions);
    check_burn_list_solutions(Ms, MsGridSize, MsBurnSums, BurnListsTail, Acc, Solutions).

car([Head|_],Head).

% This is brute force and very inefficient.
ms_find_solutions(Solutions) :-
    ms(Ms),
    ms_grid_size(MsGridSize),
    ms_burn_sums(MsBurnSums),
    car(MsBurnSums, FirstBurnSum),
    list_sum(FirstBurnSum, TotalBurnSum),
    write(TotalBurnSum), nl,
    list_lengths(Ms, MsLengths),
    maplist(list_range_0toN, MsLengths, BurnRanges),
    outer_product(BurnRanges, AllPossibleBurns),
    % outer_product_with_matching_sum(BurnRanges, TotalBurnSum, AllPossibleBurns),
    check_burn_list_solutions(Ms, MsGridSize, MsBurnSums, AllPossibleBurns, [], Solutions).

ms_is_solution(BurnList) :-
    ms(Ms),
    length(Ms, MsCount),
    length(BurnList, BurnListLength),
    MsCount = BurnListLength,
    ms_grid_size(MsGridSize),
    ms_burn_sums(MsBurnSums),
    burn_valid(Ms, MsGridSize, MsBurnSums, BurnList).
