% This is from the answer on the Mensa Calendar Puzzle from 2016-09-20. The
% matchstick pattern differs between the answer and the puzzle on the front!
% -----------------------------------------------------------------------------
% These define the characteristics of a matchstick puzzle ("ms" is for "matchstick"):
ms_grid_size([6,6]).            % This allows arbitrary-dimensional matchstick problems.
% List of matches from head to tail, horizontal then vertical.
ms_names([m1ad,m2da,m3ad,m4da,m5ad,m6da,me31,me46,mf14,mf65]).
ms_burn_sums([[3,3,3,4,5,4],[3,4,5,3,3,4]]).   % Per axis in grid_size([...]).
% Solution: [2,2,3,3,3,2,0,3,2,2].

write_lists([]).
write_lists([Head | Tail]) :- write(Head), nl, write_lists(Tail).

initialize :-
    ms_find_solutions(L),
    write_lists(L).
