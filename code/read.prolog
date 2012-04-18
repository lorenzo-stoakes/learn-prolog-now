main:-
    open('houses.txt',read,Stream),
    read(Stream,House1),
    read(Stream,House2),
    read(Stream,House3),
    read(Stream,House4),
    close(Stream),
    write([House1,House2,House3,House4]), nl.