readWord(InStream,W):-
    get_code(InStream,Char),
    checkCharAndReadRest(Char,Chars,InStream),
    atom_codes(W,Chars).

% Char newline?
checkCharAndReadRest(10,[],_):- !.

% Char blank?
checkCharAndReadRest(32,[],_):- !.

% End of stream?
checkCharAndReadRest(-1,[],_):- !.

% End of file?
checkCharAndReadRest(end_of_file,[],_):- !.

checkCharAndReadRest(Char,[Char|Chars],InStream):-
    get_code(InStream,NextChar),
    checkCharAndReadRest(NextChar,Chars,InStream).
