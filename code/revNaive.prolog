revNaive([], []).
revNaive([H|T], R):-
    revNaive(T, Rnew),
    append_(Rnew,[H],R).