enjoys(vincent,X):- \+ big_kahuna_burger(X), burger(X).

burger(X):- big_mac(X).
burger(X):- big_kahuna_burger(b).
burger(X):- whopper(X).

big_mac(a).
big_kahuna_burger(b).
big_mac(c).
whopper(d).
