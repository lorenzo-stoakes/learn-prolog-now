main:-
    open('houses.txt',read,Stream),
    read_houses(Stream,Houses),
    close(Stream),
    write(Houses), nl.

read_houses(Stream,[]):-
    at_end_of_stream(Stream), !.

read_houses(Stream,[X|L]):-
    \+ at_end_of_stream(Stream),
    read(Stream,X),
    read_houses(Stream,L).