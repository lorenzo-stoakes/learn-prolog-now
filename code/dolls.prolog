directlyIn(katarina,olga).
directlyIn(olga,natasha).
directlyIn(natasha,irina).

in(X,Y):- directlyIn(X,Y).

in(X,Y):-
    directlyIn(X,Z),
    in(Z,Y).