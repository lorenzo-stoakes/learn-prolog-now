<link href="http://kevinburke.bitbucket.org/markdowncss/markdown.css" rel="stylesheet"></link>

Learn Prolog Now!
=================

Notes for
[chapter 12](http://www.learnprolognow.org/lpnpage.php?pagetype=html&pageid=lpn-htmlch12).

I use [swipl](http://www.swi-prolog.org/) as my prolog interpreter.

Chapter 12 - Working With Files
-------------------------------

12.1 Splitting Programs over Files
----------------------------------

### Reading in Programs ###

* We already know of a way of inputting files to Prolog. Namely:-

```prolog
    [FileName].
```

* However, it's worth noting that we can read multiple files this way. E.g.:-

```prolog
    [FileName1,FileName2,...,FileNameN].
```

* We don't have to do this interactively, we can put this at the top of a program file. E.g.:-

```prolog
    :- [FileName1,FileName2,...,FileNameN].
```

* Prolog doesn't check whether a file needs to be read, i.e. if another file has referenced
  something you're attempting to load. We can work around this thus:-

```prolog
    ensure_loaded(['listPredicates.prolog']).
```

### Modules ###

* Consider writing a program which manages a movie database. E.g.:-

```prolog
    % printActors.prolog

    printActors(Film):-
        setof(Actor,starring(Actor,Film),List),
        displayList(List).

    displayList([]):- nl.
    displayList([X|L]):-
        write(X), tab(1),
        displayList(L).

    % printMovies.prolog

    printMovies(Director):-
        setof(Film,directed(Director,Film),List),
        displayList(List).

    displayList([]):- nl.
    displayList([X|L]):-
        write(X), nl,
        displayList(L).
```

* Note that __displayList__ has different definitions in each of these files, the actors are
  printed in a row (using __tab/1__), whereas the films are printed in a column (using
  __nl/0__). Will this result in a conflict? Let's see.

Defining our main Prolog file:-

```prolog
    :- ['printActors.prolog'].
    :- ['printMovies.prolog'].

    % For now we're leaving out any actual code.
```

What happens when we try to load this?

```prolog
    ?- ['main.prolog'].
    %  printActors.prolog compiled 0.00 sec, 5 clauses
    Warning: /Users/lstoakes/Dropbox/study/learnPrologNow/code/movie/printMovies.prolog:5:
    	Redefined static procedure displayList/1
    %  printMovies.prolog compiled 0.00 sec, 5 clauses
    % main.prolog compiled 0.00 sec, 14 clauses
    true.
```

* We *could* rename the predicates, however this seems an unnecessary waste of energy.

* We can work around this by using *modules*. By declaring these files as modules, we can
  define which predicates are public and which predicates are private. Modules are declared
  thusly:-

```prolog
    module(ModuleName, List_of_Predicates_to_be_Exported).
```

So, adapting our code:-

```prolog
    % printActors.prolog

    :- module(printActors,[printActors/1]).

    printActors(Film):-
        setof(Actor,starring(Actor,Film),List),
        displayList(List).

    displayList([]):- nl.
    displayList([X|L]):-
        write(X), tab(1),
        displayList(L).

    % printMovies.prolog

    :- module(printMovies,[printMovies/1]).

    printMovies(Director):-
        setof(Film,directed(Director,Film),List),
        displayList(List).

    displayList([]):- nl.
    displayList([X|L]):-
        write(X), nl,
        displayList(L).
```

* We reference our modules in __main.prolog__ using __use\_module/1__. E.g.:-

```prolog
    :- use_module('printActors.prolog').
    :- use_module('printMovies.prolog').
```

* We could also access them using __use\_module/2__, which we can use to define which
  predicates we want from the module. E.g.:-

```prolog
    :- use_module('printActors.prolog',[printActors/1]).
    :- use_module('printMovies.prolog',[printMovies/1]).
```

* Now, when we load __main.prolog__, we get quite a different response. E.g.:-

```prolog
    ?- ['main.prolog'].
    %  printActors.prolog compiled into printActors 0.00 sec, 2 clauses
    %  printMovies.prolog compiled into printMovies 0.00 sec, 4 clauses
    % main.prolog compiled 0.00 sec, 10 clauses
    true.
```

### Libraries ###

* Many of the more common predicates are provided predefined by most Prolog
  implementations. E.g. in SWI Prolog __append/3__ and __member/2__ come as part of the
  system. This is not always the case.

* We can tell Prolog to read a library (i.e. from a file kept where the implementation keeps
  its library files) using __library()__.

E.g., you could put the following at the top of your file to load the __lists__ library:-

```prolog
    :- use_module(library(lists)).
```

12.2 Writing to Files
---------------------

* If we want to write to a file rather than the screen, we need to write to a file stream.

E.g.:-

```prolog
    open('hogwarts.txt',write,Stream),
    write(Stream,'Hogwarts'), nl(Stream),
    close(Stream).
```

* Here we create + open a file 'hogwarts.txt' for writing, putting the stream value into
  __Stream__, write the string 'Hogwarts' to the file, add a newline via __nl/1__, then close
  the stream.

* We could append to a file, for example, by using __append__ as an atom instead of __write__.

12.3 Reading from Files
-----------------------

* We can read from files relatively straightforwardly, if the information contained within the
  file is given in terms of Prolog terms followed by fullstops, e.g.:-

    gryffindor.
    hufflepuff.
    ravenclaw.
    slytherin.

* We can write code to read this. E.g.:-

```prolog
    main:-
        open('houses.txt',read,Stream),
        read(Stream,House1),
        read(Stream,House2),
        read(Stream,House3),
        read(Stream,House4),
        close(Stream),
        write([House1,House2,House3,House4]), nl.

    ?- main.
    [gryffindor,hufflepuff,ravenclaw,slytherin]
    true.
```

* Can we read these without having to know the length of the file upfront? Yes we can. E.g.:-

```prolog
    main:-
        open('houses.txt',read,Stream),
        read_houses(Stream,Houses),
        close(Stream),
        write(Houses), nl.

    read_houses(Stream,[]):-
        at_end_of_stream(Stream).

    read_houses(Stream,[X|L]):-
        \+ at_end_of_stream(Stream),
        read(Stream,X),
        read_houses(Stream,L).
```

* If we want to read input without them having to be in Prolog terms, things get trickier - we
  have to read things on the level of characters. We can use __atom\_codes/2__ to convert a
  collection of integers into a corresponding atom. E.g.:-

```prolog
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
```
