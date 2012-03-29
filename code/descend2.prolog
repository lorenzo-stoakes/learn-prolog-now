child(anne,bridget).
child(bridget,caroline).
child(caroline,donna).
child(donna,emily).

descend(X,Y) :- child(X,Z),
    descend(Z,Y).
descend(X,Y) :- child(X,Y).