child(anne,bridget).
child(bridget,caroline).
child(caroline,donna).
child(donna,emily).

descend(X,Y) :- descend(Z,Y),
    child(X,Z).

descend(X,Y) :- child(X,Y).