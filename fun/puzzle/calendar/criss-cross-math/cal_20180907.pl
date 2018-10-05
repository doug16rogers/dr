% Use: GLOBALSZ=850000 LOCALSZ=524288 TRAILSZ=524288 gprolog --consult-file cal_20180907.pl
%      | ?- find_valid(L).

ccm_row_ops(["+-","--","+-"]).
ccm_row_result([9,6,6]).
ccm_col_ops(["+/","+/","/+"]).
ccm_col_result([5,1,7]).

num_valid(N) :- (N >= 1), (N <= 9).

row_valid_with_sum([], Sum, ExpectedSum) :- Sum is ExpectedSum.
row_valid_with_sum([Head | Rest], Sum, ExpectedSum) :-
    num_valid(Head),
    row_valid_with_sum(Rest, Sum + Head, ExpectedSum).

find_valid(L) :-
    ...
    
