% http://www.cpp.edu/~jrfisher/www/prolog_tutorial/2_7.html
car([X|_],X).
cdr([_|Y],Y).
cons(X,R,[X|R]).

member1(X,[X|_]).
member1(X,[_|R]) :- member1(X,R).

takeout(X,[X|R],R).
takeout(X,[F|R],[F|S]) :- takeout(X,R,S).
putin(X,L,R) :- takeout(X,R,L).

append1([X|Y],Z,[X|W]) :- append1(Y,Z,W).
append1([],X,X).

reverse1([X|Y],Z,W) :- reverse1(Y,[X|Z],W).
reverse1([],X,X).
reverse1(L,R) :- reverse1(L,[],R).

perm1([X|Y],Z) :- perm1(Y,W), takeout(X,Z,W).
perm1([],[]).

perm2([X|Y],Z) :- perm2(Y,W), putin(X,W,Z).
perm2([],[]).

% Works for instantiated lists but causes infinite regress when calling i.e.
% subset1(Subset,[1,2]) and subset1([1,2],Superset).
subset1([X|R],S) :- member(X,S), subset1(R,S).
subset1([],_).

% Trying to create a form of subset() that allows
% findall(Subset,subset2(Subset,[instantiated_list]),Subsets) to work. Of
% course, subset2([instantiated_list],Superset) will always be infinite.
subset2([X], [X|_]).
subset2([X|R], [Y|S]) :- X \== Y, member(X, S), subset2(R, [Y|S]).
subset2([],_).

% member2/2 is slower than member1/2 because it goes through R completely and
% finds the member only on the way back up.
member2(X,[_|R]) :- member2(X,R).
member2(X,[X|_]).
