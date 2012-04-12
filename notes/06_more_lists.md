<link href="http://kevinburke.bitbucket.org/markdowncss/markdown.css" rel="stylesheet"></link>

Learn Prolog Now!
=================

Notes for [chapter 6](http://www.learnprolognow.org/lpnpage.php?pagetype=html&pageid=lpn-htmlch6).

I use [swipl](http://www.swi-prolog.org/) as my prolog interpreter.

Chapter 6 - More Lists
----------------------

6.1 Append
----------

* We will define an important predicate, __append/3__, whose arguments are all lists.

* Declaratively, __append__ will hold when __L3__ in __append(L1,L2,L3)__ is equal to __L1__
  and __L2__ appended together.

E.g.:-

    ?- append([a,b,c],[1,2,3],[a,b,c,1,2,3]).
    true.

* Obviously, you're more likely to use the predicate procedurally to append two lists together.

E.g.:-

    ?- append([1,2],[3],R).
    R = [1, 2, 3].

* Actually __append__ is of considerably more use than this, and can be used to split up a list. 

Its definition is:-

    append([],L2,L2).
    append([H|T],L2,[H|L3]):-
        append(T,L2,L3).

This seems odd, but what is happening is that we're saying at each recursive step that our
final parameter unifies with something which consists of the head of the first list combined
with a tail of some value. As we recurse down we end up with H1|(H2|(H3|(...|(Hn|L2)))), and as
we return from recursive calls, all the head/tail appends are performed giving us our combined
list.

E.g. a trace:-

    [trace]  ?- append_([1,2],[3,4],R).
       Call: (6) append_([1, 2], [3, 4], _G340) ? 
       Call: (7) append_([2], [3, 4], _G419) ? 
       Call: (8) append_([], [3, 4], _G422) ? 
       Exit: (8) append_([], [3, 4], [3, 4]) ? 
       Exit: (7) append_([2], [3, 4], [2, 3, 4]) ? 
       Exit: (6) append_([1, 2], [3, 4], [1, 2, 3, 4]) ? 
    R = [1, 2, 3, 4].

* We're essentially using unification to build a structure.

### Using Append ###

* We can use __append/3__ to split up a list into two consecutive lists.

E.g.:-

    ?- append(X,Y,[a,b,c,d]).
    X = [],
    Y = [a, b, c, d] ;
    X = [a],
    Y = [b, c, d] ;
    X = [a, b],
    Y = [c, d] ;
    X = [a, b, c],
    Y = [d] ;
    X = [a, b, c, d],
    Y = [] ;
    false.

* Or to find the prefixes of a list.

E.g.:-

    prefix(P,L):- append(P,_,L).

    ?- prefix(X,[1,2,3]).
    X = [] ;
    X = [1] ;
    X = [1, 2] ;
    X = [1, 2, 3] ;
    false.

Let's look at a trace:-

    [trace]  ?- prefix_(X,[1,2,3]).
       Call: (6) prefix_(_G995, [1, 2, 3]) ? 
       Call: (7) append_(_G995, _G1075, [1, 2, 3]) ? 
       Exit: (7) append_([], [1, 2, 3], [1, 2, 3]) ? 
       Exit: (6) prefix_([], [1, 2, 3]) ? 
    X = [] ;
       Redo: (7) append_(_G995, _G1075, [1, 2, 3]) ? 
       Call: (8) append_(_G1071, _G1078, [2, 3]) ? 
       Exit: (8) append_([], [2, 3], [2, 3]) ? 
       Exit: (7) append_([1], [2, 3], [1, 2, 3]) ? 
       Exit: (6) prefix_([1], [1, 2, 3]) ? 
    X = [1] ;
       Redo: (8) append_(_G1071, _G1078, [2, 3]) ? 
       Call: (9) append_(_G1074, _G1081, [3]) ? 
       Exit: (9) append_([], [3], [3]) ? 
       Exit: (8) append_([2], [3], [2, 3]) ? 
       Exit: (7) append_([1, 2], [3], [1, 2, 3]) ? 
       Exit: (6) prefix_([1, 2], [1, 2, 3]) ? 
    X = [1, 2] ;
       Redo: (9) append_(_G1074, _G1081, [3]) ? 
       Call: (10) append_(_G1077, _G1084, []) ? 
       Exit: (10) append_([], [], []) ? 
       Exit: (9) append_([3], [], [3]) ? 
       Exit: (8) append_([2, 3], [], [2, 3]) ? 
       Exit: (7) append_([1, 2, 3], [], [1, 2, 3]) ? 
       Exit: (6) prefix_([1, 2, 3], [1, 2, 3]) ? 
    X = [1, 2, 3] ;
       Redo: (10) append_(_G1077, _G1084, []) ? 
       Fail: (10) append_(_G1077, _G1084, []) ? 
       Fail: (9) append_(_G1074, _G1081, [3]) ? 
       Fail: (8) append_(_G1071, _G1078, [2, 3]) ? 
       Fail: (7) append_(_G995, _G1075, [1, 2, 3]) ? 
       Fail: (6) prefix_(_G995, [1, 2, 3]) ? 
    false.

* And the suffixes.

E.g.:-

    suffix(S,L):- append(_,S,L).

    ?- suffix(X,[1,2,3]).
    X = [1,2,3] ;
    X = [2,3] ;
    X = [3] ;
    X = [] ;
    false.

Again, a trace:-

    [trace]  ?- suffix_(X,[1,2,3]).
       Call: (6) suffix_(_G335, [1, 2, 3]) ? 
       Call: (7) append_(_G414, _G335, [1, 2, 3]) ? 
       Exit: (7) append_([], [1, 2, 3], [1, 2, 3]) ? 
       Exit: (6) suffix_([1, 2, 3], [1, 2, 3]) ? 
    X = [1, 2, 3] ;
       Redo: (7) append_(_G414, _G335, [1, 2, 3]) ? 
       Call: (8) append_(_G411, _G335, [2, 3]) ? 
       Exit: (8) append_([], [2, 3], [2, 3]) ? 
       Exit: (7) append_([1], [2, 3], [1, 2, 3]) ? 
       Exit: (6) suffix_([2, 3], [1, 2, 3]) ? 
    X = [2, 3] ;
       Redo: (8) append_(_G411, _G335, [2, 3]) ? 
       Call: (9) append_(_G414, _G335, [3]) ? 
       Exit: (9) append_([], [3], [3]) ? 
       Exit: (8) append_([2], [3], [2, 3]) ? 
       Exit: (7) append_([1, 2], [3], [1, 2, 3]) ? 
       Exit: (6) suffix_([3], [1, 2, 3]) ? 
    X = [3] ;
       Redo: (9) append_(_G414, _G335, [3]) ? 
       Call: (10) append_(_G417, _G335, []) ? 
       Exit: (10) append_([], [], []) ? 
       Exit: (9) append_([3], [], [3]) ? 
       Exit: (8) append_([2, 3], [], [2, 3]) ? 
       Exit: (7) append_([1, 2, 3], [], [1, 2, 3]) ? 
       Exit: (6) suffix_([], [1, 2, 3]) ? 
    X = [] ;
       Redo: (10) append_(_G417, _G335, []) ? 
       Fail: (10) append_(_G417, _G335, []) ? 
       Fail: (9) append_(_G414, _G335, [3]) ? 
       Fail: (8) append_(_G411, _G335, [2, 3]) ? 
       Fail: (7) append_(_G414, _G335, [1, 2, 3]) ? 
       Fail: (6) suffix_(_G335, [1, 2, 3]) ? 
    false.

* We can also define a __sublist__ predicate, using the follows:-

    sublist(SubL,L):- suffix(S,L), prefix(SubL,S).

I.e., __SubL__ is a sublist of __L__ if there is some suffix __S__ of __L__ of which __SubL__
is a prefix. The program isn't explicitly using __append/3__, but under the covers, that's what
it's using.

6.2 Reversing a List
--------------------

* __append/3__ is useful, and it's important to know how to use it. But it's important to
  realise that it can also be a source of inefficiency, and that you probably don't want to use
  it all the time.

* The reason for this potential source of inefficiency is that __append/3__ needs to recurse
  down the first list argument until it finds the start of the list, and only then can it carry
  out the concatenation.

* Generally speaking, this doesn't cause issues, but things are very different when you give
  __append/3__ variables in the first two arguments, because this causes Prolog to search for
  ways to split up a list, which can involve a lot of work.

* To illustrate this limitation, we will look at a predicate which takes a list, and returns
  the same list in reverse order:-

This is based on the idea that we need some procedure which:-

1. When the input list is empty, simply returns the empty list.
2. Otherwise takes advantage of the fact that for a list [H|T], the reverse of the list is
rev(T) + H.

This is implemented as such:-

    revNaive([], []).
    revNaive([H|T], R):-
        revNaive(T, Rnew),
        append_(Rnew,[H],R).

Let's look at a trace of this running:-

    [trace]  ?- revNaive([1,2,3],R).
       Call: (6) revNaive([1, 2, 3], _G542) ? 
       Call: (7) revNaive([2, 3], _G624) ? 
       Call: (8) revNaive([3], _G624) ? 
       Call: (9) revNaive([], _G624) ? 
       Exit: (9) revNaive([], []) ? 
       Call: (9) append_([], [3], _G628) ? 
       Exit: (9) append_([], [3], [3]) ? 
       Exit: (8) revNaive([3], [3]) ? 
       Call: (8) append_([3], [2], _G631) ? 
       Call: (9) append_([], [2], _G626) ? 
       Exit: (9) append_([], [2], [2]) ? 
       Exit: (8) append_([3], [2], [3, 2]) ? 
       Exit: (7) revNaive([2, 3], [3, 2]) ? 
       Call: (7) append_([3, 2], [1], _G542) ? 
       Call: (8) append_([2], [1], _G632) ? 
       Call: (9) append_([], [1], _G635) ? 
       Exit: (9) append_([], [1], [1]) ? 
       Exit: (8) append_([2], [1], [2, 1]) ? 
       Exit: (7) append_([3, 2], [1], [3, 2, 1]) ? 
       Exit: (6) revNaive([1, 2, 3], [3, 2, 1]) ? 
    R = [3, 2, 1].

* We're better off using an accumulator, as previously implemented :-) :-

    rev([], R, R).

    rev([H|T], A, R):-
        rev(T, [H|A], R).

    rev(L, R):-
        rev(L, [], R).

Under trace:-

    [trace]  ?- rev([1,2,3],R).
       Call: (6) rev([1, 2, 3], _G820) ? 
       Call: (7) rev([1, 2, 3], [], _G820) ? 
       Call: (8) rev([2, 3], [1], _G820) ? 
       Call: (9) rev([3], [2, 1], _G820) ? 
       Call: (10) rev([], [3, 2, 1], _G820) ? 
       Exit: (10) rev([], [3, 2, 1], [3, 2, 1]) ? 
       Exit: (9) rev([3], [2, 1], [3, 2, 1]) ? 
       Exit: (8) rev([2, 3], [1], [3, 2, 1]) ? 
       Exit: (7) rev([1, 2, 3], [], [3, 2, 1]) ? 
       Exit: (6) rev([1, 2, 3], [3, 2, 1]) ? 
    R = [3, 2, 1].
