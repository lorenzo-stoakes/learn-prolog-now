<link href="http://kevinburke.bitbucket.org/markdowncss/markdown.css" rel="stylesheet"></link>

Learn Prolog Now!
=================

Notes for
[chapter 11](http://www.learnprolognow.org/lpnpage.php?pagetype=html&pageid=lpn-htmlch11).

I use [swipl](http://www.swi-prolog.org/) as my prolog interpreter.

Chapter 11 - Database Manipulation
----------------------------------

11.1 Database Manipulation
--------------------------

* There are four database manipulation commands.

E.g.:-

    assert
    retract
    asserta
    assertz

* If we start with an empty database, and give the __listing__ command, we get the following.

E.g.:-

    ?- listing.
    true.

* Now suppose we give the following command.

E.g.:-

    ?- assert(happy(mia)).
    true.

* What's interesting here is not that it succeeds, but rather its side-effect. If we now try
  the __listing__ command, we get:-

E.g.:-

    ?- listing.

    :- dynamic happy/1.

    happy(mia).
    true.

* So __happy(mia)__ has now entered into the database. Let's try some more.

E.g.:-

    ?- assert(happy(vincent)).
    true.

    ?- assert(happy(marcellus)).
    true.

    ?- assert(happy(butch)).
    true.

    ?- assert(happy(vincent)).
    true.

    ?- listing.

    :- dynamic happy/1.

    happy(mia).
    happy(vincent).
    happy(marcellus).
    happy(butch).
    happy(vincent).
    true.

* So all the facts that we've asserted are now in the knowledge base. Note that
  __happy(vincent)__ is in the KB twice. As we asserted it twice, this makes sense.

* The database manipulations we've made have changed the meaning of the predicate __happy/1__.

* Generally speaking, database commands give us the ability to change the meaning of predicates
  while we are running programs.

* Predicates whose definitions change during runtime are called *dynamic* predicates, as
  opposed to *static* predicates, i.e. all the other predicates we have run into so far.

* Most Prolog implementations require that the predicates we wish to be dynamic be specifically
  declared that way.

* Let's say we want to assert the rule that everybody who is happy is naive.

E.g.:-

    naive(X):- happy(X).

We can do this via:-

    assert((naive(X):- happy(X))).

Note that we place the rule we're asserting in parentheses.

* If we ask for a listing we see that this has been added.

E.g.:-

    ?- listing.

    :- dynamic naive/1.

    naive(A) :-
    	happy(A).

    :- dynamic happy/1.

    happy(mia).
    happy(vincent).
    happy(marcellus).
    happy(butch).
    happy(vincent).
    true.

* We can also __retract__ things from the database.

E.g.:-

    ?- retract(happy(marcellus)).
    true.

    ?- listing.

    :- dynamic naive/1.

    naive(A) :-
    	happy(A).

    :- dynamic happy/1.

    happy(mia).
    happy(vincent).
    happy(butch).
    happy(vincent).
    true.

And if we remove __happy(vincent)__:-

    ?- retract(happy(vincent)).
    true .

    ?- listing.

    :- dynamic naive/1.

    naive(A) :-
    	happy(A).

    :- dynamic happy/1.

    happy(mia).
    happy(butch).
    happy(vincent).
    true.

* Note that only the first occurrence of __happy(vincent)__ was removed.

* If we want to remove all of our assertions for a given predicate, we can use a variable.

E.g.:-

    ?- retract(happy(X)).
    X = mia ;
    X = butch ;
    X = vincent.

    ?- listing.

    :- dynamic naive/1.

    naive(A) :-
    	happy(A).

    :- dynamic happy/1.

    true.

* If we want to control where the asserted material is placed, we can use __assertz__ and
  __asserta__. __assertz__ places asserted material at the end of the database, whereas
  __asserta__ places it at the beginning.

E.g.:-

    ?- assert(p(b)),assertz(p(c)),asserta(p(a)).
    true.

    ?- listing.

    :- dynamic p/1.

    p(a).
    p(b).
    p(c).
    true.

* We can use database manipulation for *memoisation*, i.e. caching results between
  computations.

E.g.:-

    :- dynamic lookup/3.

    add_and_square(X,Y,Res):-
        lookup(X,Y,Res), !.

    add_and_square(X,Y,Res):-
        Res is (X+Y)*(X+Y),
        assert(lookup(X,Y,Res)).

Now let's try some queries:-

    ?- add_and_square(3,7,X).
    X = 100.

    ?- add_and_square(3,4,Y).
    Y = 49.

* If we run a listing, we'll see these memoised results.

E.g.:-

    ?- listing.

    :- dynamic lookup/3.

    lookup(3, 7, 100).
    lookup(3, 4, 49).

    add_and_square(A, B, C) :-
    	lookup(A, B, C), !.
    add_and_square(B, C, A) :-
    	A is (B+C)* (B+C),
    	assert(lookup(B, C, A)).
    true.

* We can remove these from the database without having to be prompted each time by using the
  __retractall__ predicate.

E.g.:-

    ?- retractall(lookup(_,_,_)).
    true.

* Be careful with manipulating the database - this can lead to confusing, messy programs.

11.2 Collecting Solutions
-------------------------

* Often, there can be multiple solutions to a query.

E.g.:-

    child(martha,charlotte).
    child(charlotte,caroline).
    child(caroline,laura).
    child(laura,rose).

    descend(X,Y):- child(X,Y).
    descend(X,Y):- child(X,Z), descend(Z,Y).

If we run the query:-

    ?- descend(martha, X).

We get multiple queries, one-by-one, which we have to iterate through, e.g.:-

    ?- descend(martha,X).
    X = charlotte ;
    X = caroline ;
    X = laura ;
    X = rose ;
    false.

* It would be nice to have all of these results in one go in a neat, usable form. Prolog has 3
  predicates which can help us with this - __findall__, __bagof__, and __setof__. Essentially,
  all these predicates collect all the solutions from a query and put them into a single
  list. Having said that, there are important differences between them which we should be aware
  of.

### The findall/3 Predicate ###

* The job of __findall__ is to provide a list __List__ of the objects __Object__ that satisfy
  the goal __Goal__. Often __Object__ is a variable, in which case __findall__ finds all
  instantiations of __Object__ which satisfy __Goal__.

E.g.:-

    ?- findall(X,descend(martha,X),L).
    L = [charlotte, caroline, laura, rose].

* __Object__ doesn't have to be a variable, it might be a complex term which contains a
  variable that also occurs in __Goal__.

E.g.:-

    ?- findall(fromMartha(X),descend(martha,X),L).
    L = [fromMartha(charlotte), fromMartha(caroline), fromMartha(laura), fromMartha(rose)].

* What if we ask __findall__ for a situation which cannot be satisfied?

E.g.:-

    ?- findall(X,descend(mary,X),L).
    L = [].

We simply get the empty list back.

* Typically, we want to share variables between the first two arguments of __findall__, as we
  are actually interested in the solutions which satisfy the goal. However, this is not always
  the case. Perhaps we are only interested in the number of descendants Martha has.

E.g.:-

    ?- findall(Y,descend(martha,X),Z),length(Z,N).
    Z = [_G991, _G988, _G985, _G982],
    N = 4.

### The bagof/3 Predicate ###

* The __findall/3__ predicate is useful, but somewhat crude.

E.g.:-

    ?- findall(Child,descend(Mother,Child),List).
    List = [charlotte, caroline, laura, rose, caroline, laura, rose, laura, rose|...].

* This is correct (though truncated), however it'd be nice to have a separate list for each
  different instantiation of __Mother__. __bagof__ will let us do this.

E.g.:-

    ?- bagof(Child,descend(Mother,Child),List).
    Mother = caroline,
    List = [laura, rose] ;
    Mother = charlotte,
    List = [caroline, laura, rose] ;
    Mother = laura,
    List = [rose] ;
    Mother = martha,
    List = [charlotte, caroline, laura, rose].

* This tells us each set of instantiations we require. It's possible to revert __bagof__'s
  behaviour to that of __findall__ if we require.

E.g.:-

    ?- bagof(Child,Mother^descend(Mother,Child),List).
    List = [charlotte, caroline, laura, rose, caroline, laura, rose, laura, rose|...].

Here we've used the __^__ operator to say we don't care about __Mother__
instantiations. Obviously, in a situation like this you'd be better off simply using
__findall__, but this option is there, and obviously you can selectively choose which variables
you don't care about.

* It'd be nice if we could get the best of both worlds - see the individual instantiations we
  require to get the results we're interested in *and* collect everything into one list.

We can, e.g.:-

    ?- findall(List,bagof(Child,descend(Mother,Child),List),Z).
    Z = [[laura, rose], [caroline, laura, rose], [rose], [charlotte, caroline, laura, rose]].

We can do this with __bagof__ too:-

    ?- bagof(List,Child^Mother^bagof(Child,descend(Mother,Child),List),Z).
    Z = [[laura, rose], [caroline, laura, rose], [rose], [charlotte, caroline, laura, rose]].

* Note that __bagof/3__ will return __false__ if the query fails, unlike __findall/3__ which
  will return the empty list.

E.g.:-

    ?- findall(X,descend(mary,X),L).
    L = [].

    ?- bagof(X,descend(mary,X),L).
    false.

### The setof/3 Predicate ###

* The __setof/3__ is the same as __bagof/3__, only it returns ordered lists with no duplicates.

E.g., consider the following KB:-

    age(harry,13).
    age(draco,14).
    age(ron,13).
    age(hermione,13).
    age(dumbledore,60).
    age(hagrid,30).

* Now let's say we wanted to get the list of people whose age is recorded in the database.

We could do this via:-

    ?- findall(X,age(X,Y),L).
    L = [harry, draco, ron, hermione, dumbledore, hagrid].

* However, perhaps we wanted this list to be sorted, in that case we can use __setof__.

E.g.:-

    ?- setof(X,Y^age(X,Y),L).
    L = [draco, dumbledore, hagrid, harry, hermione, ron].

Again, we use the __^__ operator to indicate that we're not interested in __Y__.

* Now, suppose that we want to get the ages in the database.

Again, we can do this with findall:-

    ?- findall(Y,age(X,Y),L).
    L = [13, 14, 13, 13, 60, 30].

However, this is messy - duplicated values and no ordering.

* Let's try this with __setof__.

E.g.:-

    ?- setof(Y,X^age(X,Y),L).
    L = [13, 14, 30, 60].

* As with __bagof__, __setof__ will return __false__ rather than the empty list if the goal
  cannot be fulfilled.
