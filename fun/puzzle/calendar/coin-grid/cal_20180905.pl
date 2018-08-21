% Use: GLOBALSZ=850000 LOCALSZ=524288 TRAILSZ=524288 gprolog --consult-file cal_20180905.pl
%      | ?- grid_valid(G).
% It took ~60s on an MBP but it did find the solution:
% G = [[5,10,25,0,10],[10,10,25,10,10],[5,0,25,5,10],[0,10,10,5,10],[10,5,10,10,0]] ?

% -1 means undefined - that's the puzzle!
cg_preset([[ 5, -1, 25,  0, -1],
           [-1, 10, 25, -1, -1],
           [-1, -1, -1, -1, 10],
           [-1, 10, -1,  5, -1],
           [-1, -1, -1, -1, -1]]).
cg_row_sum([50,65,45,35,35]).
cg_col_sum([30,35,95,30,40]).
cg_diag_sum([45,65]).

coin_valid(0).
coin_valid(5).
coin_valid(10).
coin_valid(25).

% @TODO: Remove the 5x5 hard-coding.

% -----------------------------------------------------------------------------
%% transpose() from https://stackoverflow.com/questions/4280986/how-to-transpose-a-matrix-in-prolog?rq=1.
%% transpose([], []).
%% transpose([F|Fs], Ts) :-
%%     transpose(F, [F|Fs], Ts).

%% transpose([], _, []).
%% transpose([_|Rs], Ms, [Ts|Tss]) :-
%%     lists_firsts_rests(Ms, Ts, Ms1),
%%     transpose(Rs, Ms1, Tss).

%% lists_firsts_rests([], [], []).
%% lists_firsts_rests([[F|Os]|Rest], [F|Fs], [Os|Oss]) :-
%%     lists_firsts_rests(Rest, Fs, Oss).

% Quicker cheat for 5x5:
transpose5(M, T) :-
    M = [[M11, M12, M13, M14, M15],
         [M21, M22, M23, M24, M25],
         [M31, M32, M33, M34, M35],
         [M41, M42, M43, M44, M45],
         [M51, M52, M53, M54, M55]],
    T = [[M11, M21, M31, M41, M51],
         [M12, M22, M32, M42, M52],
         [M13, M23, M33, M43, M53],
         [M14, M24, M34, M44, M54],
         [M15, M25, M35, M45, M55]].

% To test:
%% transpose2(M, T) :-
%%     M = [[M11, M12], [M21, M22]],
%%     T = [[M11, M21], [M12, M22]] .

transpose(M, T) :- transpose5(M, T).

% -----------------------------------------------------------------------------

% If there is no preset then any valid coin matches.
coin_valid_with_preset(Coin, -1) :- coin_valid(Coin).
% No need to check coin_valid() since presumably the preset it correct.
coin_valid_with_preset(Coin, Preset) :- Coin is Preset.

coin_n_valid_with_preset(N, L, PresetList, Coin) :-
    nth(N, L, Coin),
    nth(N, PresetList, Preset),
    coin_valid_with_preset(Coin, Preset).

list_n_valid(N, Grid, Preset, Sums) :-
    nth(N, Grid, List),
    nth(N, Preset, PresetList),
    coin_n_valid_with_preset(1, List, PresetList, Coin1),
    coin_n_valid_with_preset(2, List, PresetList, Coin2),
    coin_n_valid_with_preset(3, List, PresetList, Coin3),
    coin_n_valid_with_preset(4, List, PresetList, Coin4),
    coin_n_valid_with_preset(5, List, PresetList, Coin5),
    nth(N, Sums, Sum),
    Sum is Coin1 + Coin2 + Coin3 + Coin4 + Coin5.

rows_valid(Grid) :-
    cg_preset(PresetGrid),
    cg_row_sum(Sums),
    list_n_valid(1, Grid, PresetGrid, Sums),
    list_n_valid(2, Grid, PresetGrid, Sums),
    list_n_valid(3, Grid, PresetGrid, Sums),
    list_n_valid(4, Grid, PresetGrid, Sums),
    list_n_valid(5, Grid, PresetGrid, Sums).

cols_valid(Grid) :-
    cg_preset(PresetGrid),
    transpose(PresetGrid, PresetGird),
    cg_col_sum(Sums),
    list_n_valid(1, Gird, PresetGird, Sums),
    list_n_valid(2, Gird, PresetGird, Sums),
    list_n_valid(3, Gird, PresetGird, Sums),
    list_n_valid(4, Gird, PresetGird, Sums),
    list_n_valid(5, Gird, PresetGird, Sums),
    transpose(Grid, Gird).    % Putting this at the end allowed cols_valid(G) to run without exploding.

diags_valid(Grid) :-
    nth(1, Grid, Row1),
    nth(2, Grid, Row2),
    nth(3, Grid, Row3),
    nth(4, Grid, Row4),
    nth(5, Grid, Row5),
    nth(1, Row1, Coin11), coin_valid(Coin11),
    nth(2, Row2, Coin22), coin_valid(Coin22),
    nth(3, Row3, Coin33), coin_valid(Coin33),
    nth(4, Row4, Coin44), coin_valid(Coin44),
    nth(5, Row5, Coin55), coin_valid(Coin55),

    cg_diag_sum(Sums),
    nth(1, Sums, ForwardSum),
    ForwardSum is Coin11 + Coin22 + Coin33 + Coin44 + Coin55,

    nth(5, Row1, Coin15), coin_valid(Coin15),
    nth(4, Row2, Coin24), coin_valid(Coin24),
%%  nth(3, Row3, Coin33), coin_valid(Coin33),
    nth(2, Row4, Coin42), coin_valid(Coin42),
    nth(1, Row5, Coin51), coin_valid(Coin51),

    nth(2, Sums, BackwardSum),
    BackwardSum is Coin15 + Coin24 + Coin33 + Coin42 + Coin51.

grid_valid(G) :-
    diags_valid(G),     % Faster with this not at the end.
    rows_valid(G),
    cols_valid(G).

