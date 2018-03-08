
sorted_sublist([], []).
sorted_sublist([Head | Tail], [Head | [SubListHead | SubListTail]]) :-
    Head =<= SubListHead,
    sorted_sublist(Tail, [SubListHead | SubListTail]).
