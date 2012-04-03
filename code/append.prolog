append_([],L2,L2).
append_([H|T],L2,[H|L3]):-
    append_(T, L2, L3).
