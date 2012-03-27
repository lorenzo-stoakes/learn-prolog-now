add(0,Y,Y).
add(succ(X),Y,succ(Z)) :-
          add(X,Y,Z).