len([], 0).
len([_|T], N):- len(T,X), N is X+1.