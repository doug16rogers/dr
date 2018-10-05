% This is the erroneous puzzle posted in Mensa Puzzle Calender for
% 2016-09-20. The matchstick pattern differs from that in the solution. The
% puzzle below has no solutions.
% -----------------------------------------------------------------------------
% These define the characteristics of a matchstick puzzle ("ms" is for
% "matchstick"):
ms_grid_size([6,6]).            % This allows arbitrary-dimensional matchstick problems.
ms_names([m1ab, m1dc, m1ef, m2fc, m3df, m4ed, m5de, m6cf, ma62, mb26, mc53, mf54]).
ms_burn_sums([[3,3,3,4,5,4],[3,4,5,3,3,4]]).   % Per axis in grid_size([...]).

write_lists([]).
write_lists([Head | Tail]) :- write(Head), nl, write_lists(Tail).

initialize :-
    ms_find_solutions(L),
    write_lists(L).
