:- dynamic lookup/3.

add_and_square(X,Y,Res):-
    lookup(X,Y,Res), !.

add_and_square(X,Y,Res):-
    Res is (X+Y)*(X+Y),
    assert(lookup(X,Y,Res)).