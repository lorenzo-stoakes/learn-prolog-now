rev([], R, R).

rev([H|T], A, R):-
    rev(T, [H|A], R).

rev(L, R):-
    rev(L, [], R).