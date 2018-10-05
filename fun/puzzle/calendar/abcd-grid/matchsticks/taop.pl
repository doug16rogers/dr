% From 'The Art of Prolog' as taken from
% http://stackoverflow.com/questions/13162803/whats-the-sld-tree-for-this-query.

natural_number(0).
natural_number(s(X)) :- natural_number(X).

plus(X, 0, X) :- natural_number(X).
plus(X, s(Y), s(Z)) :- plus(X, Y, Z).
