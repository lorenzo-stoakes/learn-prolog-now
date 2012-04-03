rev([], R, R).
rev([H|T], A, R):-
    rev(T, [H|A], R).

addOne([],A,R):-
    rev(A,[],R).
addOne([H|T], A, R):-
    N is H+1,
    addOne(T, [N|A], R).
