accMin([], A, A).
accMin([H|T], A, R):-
    H < A,
    accMin(T, H, R).
accMin([H|T], A, R):-
    H >= A,
    accMin(T, A, R).
min([H|T], R):-
    accMin(T, H, R).