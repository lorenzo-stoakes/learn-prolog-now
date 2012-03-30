member(X, [X|T]).
member(X, [_|T]):- member(X,T).