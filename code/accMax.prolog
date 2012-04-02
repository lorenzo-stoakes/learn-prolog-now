accMax([], A, A).
accMax([H|T], A, M):- H > A,  accMax(T, H, M).
accMax([H|T], A, M):- H =< A, accMax(T, A, M).

max([H|T], M):-
    accMax([H|T],H,M).

max2(List, M):-
    List = [H|_],
    accMax(List,H,M).