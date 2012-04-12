complexterm(X):-
    nonvar(X),
    functor(X,_,A),
    A > 0.