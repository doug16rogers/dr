% -----------------------------------------------------------------------------
% Using the following:
%    +-----------+-----------+-----------+
%    |11         |12 _____   |13  ___    |
%    | +------------/#####\  |   |   |   |
%    | |         ##########| |   |   |   |  1
%    | +------------\#####/  |   |   |   |
%    |           |           |   |   |   |
%    +-----------+-----------+---|   |---+
%    |21  ___    |22 _____   |23 |###|   |
%    |   |###|   |  /     \  |   |###|   |
%    |   |###|   | |       | |   |###|   |  2
%    |   |###|   |  \     /  |   |###|   |
%    |   |###|   |   |   |   |   |###|   |
%    +---|###|---+---|   |---+---|###|---+
%    |31 |###|   |32 |   |   |33 |###|   |
%    |  /#####\  |   |   |   |  /#####\  |
%    | |#######| |   |   |   | |#######| |  2
%    |  \#####/  |   |___|   |  \#####/  |
%    |           |           |           |
%    +-----------+-----------+-----------+
%       2           1           2
% Solution is:
%   [1,2,0,2]    (m1ab:1, ma32:2, mb23:0, mc13:2)
% -----------------------------------------------------------------------------
% These define the characteristics of a matchstick puzzle ("ms" is for "matchstick"):
ms_grid_size([3,3]).            % This allows arbitrary-dimensional matchstick problems.
% List of matches with names that correspond to the row/col plus the head/tail of the
% match.
ms_names([m1ba, ma32, mb23, mc31]).
ms_burn_sums([[1,2,2],[2,1,2]]).   % Per axis in ms_grid_size([...]).

% To run this in the gprolog interpreter:
% | ?- [ms_demo1].
% | ?- [ms_solve_full_brute_force].
% | ?- ms_find_solutions(L).

write_lists([]).
write_lists([Head | Tail]) :- write(Head), nl, write_lists(Tail).

main :-
    ms_find_solutions(L),
    write_lists(L).
