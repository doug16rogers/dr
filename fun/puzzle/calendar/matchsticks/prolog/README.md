Matchsticks Prolog Code
=======================

This code provides a solver for matchsticks problems as they are found in the
Mensa Puzzle Calendar. This was inspired by my inability to solve the
20160920 puzzle - it looked logically impossible. I decided to write some
code to solve matchstick puzzles in general then to apply that solver to the
20160920 puzzle.

I succeeded in building the program and applying it to that puzzle, which
fails to find a solution. It turns out that, while the burn sums for each row
and column do indeed match the solution on the back, and the solution on the
back is consistent with matchstick burns, the pattern of matchsticks in the
puzzle does NOT match the pattern of matchsticks in the solution!

By the way, I'm trying to use "matchsticks" rather than "matches" because the
latter has other meanings in this sort of problem.

Brute Force Solution Methods
----------------------------

The initial solver uses brute force over all possible segment burns of
matchsticks, even ones where the number of segments being burned is not
consistent with the total number of segments burned in the full puzzle. I
call this method "full brute force."

Another method would be to see how many segments are burned in total by
summing the row burn sums or the column burn sums, then partitioning that
number over the N matchsticks in the puzzle. I call this method "partition
brute force."

To Do
-----

Here are some things that I'd like to do to improve this:

- Use the total burn count partitioned across the matchsticks for a more
  efficient brute force method.
- Have the compiled code automatically run the initialization function.
- Allow specifying the matchsticks and burn sums on the command line.
- Determine the grid size from the matchsticks.
- Look at creeping to the answer by bumping up matchstick burn segment counts
  until the row/col burn sums are exceeded. Perhaps this is the Prolog Way?
- Try using the Finite Domain solver that's part of GNU Prolog.
- Try using the Z3 constraint solver (not Prolog at all).
