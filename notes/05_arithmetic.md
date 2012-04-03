<link href="http://kevinburke.bitbucket.org/markdowncss/markdown.css" rel="stylesheet"></link>

Learn Prolog Now!
=================

Notes for [chapter 5](http://www.learnprolognow.org/lpnpage.php?pagetype=html&pageid=lpn-htmlch5).

I use [swipl](http://www.swi-prolog.org/) as my prolog interpreter.

Chapter 5 - Arithmetic
----------------------

### 5.1 Arithmetic in Prolog ###

* Prolog provides a number of tools for manipulating integers, and also real numbers (though we
  don't look at them here.)

* Comparison between standard arithmetic and prolog equivalents:-

    +--------------------+--------------------+
    |Arithmetic Examples |  Prolog Notation   |
    +--------------------+--------------------+
    |     6 + 2 = 8      |     8 is 6+2.      |
    +--------------------+--------------------+
    |      6*2 = 12      |    12 is 6*12.     |
    +--------------------+--------------------+
    |      6-2 = 4       |     4 is 6-2.      |
    +--------------------+--------------------+
    |      6-8 = -2      |     -2 is 6-8.     |
    +--------------------+--------------------+
    |      6/2 = 3       |     3 is 6/2.      |
    +--------------------+--------------------+
    |      7/2 = 3       |     3 is 7/2.      |
    +--------------------+--------------------+
    |  1 is rem of 7/2   |   1 is mod(7,2).   |
    +--------------------+--------------------+

E.g.:-

    ?- 8 is 6+2.
    true.

    ?- 12 is 6*2.
    true.

etc.

* We can solve problems using variables, e.g.:-

    ?- X is 6+2.
    X = 8.

    ?- R is mod(7,2).
    R = 1.

* We can use arithmetic operations when we define predicates, e.g.:-

    add_3_and_double(X,Y) :- Y is (X+3)*2.

* Which gives us:-

    ?- add_3_and_double(1,X).
    X = 8.

* Standard precedence applies in Prolog, e.g.:-

    ?- X is 3+2*4.
    X = 11.

### 5.2 A Closer Look ###

* We've gone over the basics, but we need to know more.

* The most important point to grasp is that none of +,*,-,/,mod actually carry out any
  arithmetic. In fact expressions such as 3+2, 3-2 and 3*2 are simply terms.

* The functors of these terms are +, - and * respectively, and the arguments are 3 and 2. Other
  than the fact that the functors go between their arguments, these are ordinary Prolog terms
  and unless we do something special, Prolog will not actually perform any arithmetic. E.g.:-

    ?- X = 3+2.
    X = 3+2.

* All that has happened here is that __X__ has been unified with 3+2. It has done what it
  typically does when __=/2__ is used - performed unification. Similarly:-

    ?- 3+2*5 = X.
    X = 3+2*5.

* To force Prolog to actually evaluate arithmetic expressions, we have to use __is__. This
  essentially makes Prolog do something different to its normal behaviour. It's designed to
  unify variables to structures, so actually evaluating arithmetic is quite a switch.

* There are some restrictions on this capability. Firstly, the arithmetic expression must
  appear on the righthand side of __is__.

* Moreover, though we are free to use variables on the righthand side of __is__, the variable
  must have already been instantiated to a variable-free arithmetic expression. If this isn't
  the case, Prolog will provide an __instantiation\_error__. E.g.:-

    add_3_and_double(X,Y) :- Y is (X+3)*2.

    ?- add_3_and_double(X,12).
    ERROR: is/2: Arguments are not sufficiently instantiated

* For Prolog, __3+2__ is just a term. In fact, it is actually just __+(3,2)__, so e.g.:-

    X is +(3,2).

* Moreover, __is__ is just a term in the precise same way, so you can express the above query
  as:-

    is(X,+(3,2)).
    X = 5.

* Something to note here is that bolting on arithmetic to Prolog has further widened the gap
  between procedural and declarative meanings of Prolog programs.

### 5.3 Arithmetic and Lists ###

* Perhaps the most important use of arithmetic (at least, in this book), is to determine facts
  about data-structures such as lists.

* How long is a list?

Here's a recursive definition:-

1. The empty list has length 0.
2. A non-empty list has length 1 + len(Tail).

E.g.:-

    len([], 0).
    len([_|T], N), len(T,X), N is X+1.

    ?- len([1,2,3],N).
    N = 3.

* This is relatively easy to understand. However, there is an alternative approach, using
  *accumulators*. Accumulator is the Prolog analog to variables used to hold intermediate
  results found in other languages, e.g.:-

    accLen([_|T],A,L):- Anew is A+1, accLen(T, Anew, L).
    accLen([],A,A).

* Let's run a trace on this:-

    [trace]  ?- accLen([a,b,c],0,L).
       Call: (6) accLen([a, b, c], 0, _G357) ?
       Call: (7) _G442 is 0+1 ?
       Exit: (7) 1 is 0+1 ?
       Call: (7) accLen([b, c], 1, _G357) ?
       Call: (8) _G445 is 1+1 ?
       Exit: (8) 2 is 1+1 ?
       Call: (8) accLen([c], 2, _G357) ?
       Call: (9) _G448 is 2+1 ?
       Exit: (9) 3 is 2+1 ?
       Call: (9) accLen([], 3, _G357) ?
       Exit: (9) accLen([], 3, 3) ?
       Exit: (8) accLen([c], 2, 3) ?
       Exit: (7) accLen([b, c], 1, 3) ?
       Exit: (6) accLen([a, b, c], 0, 3) ?
    L = 3.

* Why do we prefer this to our previous approach? This approach is *tail recursive*, i.e. when
  we reach the bottom of the recursion we have the answer we need, which simply needs to be
  passed up the stack, whereas in __len__, we need to calculations coming back up the
  stack. There is essentially less bookkeeping, so tail recursive calls are more efficient.

* We can bring this point home by having a look at a trace of __len__:-

    [trace]  ?- len([a,b,c],L).
       Call: (6) len([a, b, c], _G356) ?
       Call: (7) len([b, c], _G438) ?
       Call: (8) len([c], _G438) ?
       Call: (9) len([], _G438) ?
       Exit: (9) len([], 0) ?
       Call: (9) _G443 is 0+1 ?
       Exit: (9) 1 is 0+1 ?
       Exit: (8) len([c], 1) ?
       Call: (8) _G446 is 1+1 ?
       Exit: (8) 2 is 1+1 ?
       Exit: (7) len([b, c], 2) ?
       Call: (7) _G356 is 2+1 ?
       Exit: (7) 3 is 2+1 ?
       Exit: (6) len([a, b, c], 3) ?
    L = 3.

* Accumulators are very common in Prolog.

### 5.4 Comparing Integers ###

* Some Prolog arithmetic predicates do actually carry out arithmetic without the use of __is__,
  i.e. the operators which compare integers:-

    +--------------------+--------------------+
    |Arithmetic Examples |  Prolog Notation   |
    +--------------------+--------------------+
    |       x < y        |       X < Y.       |
    +--------------------+--------------------+
    |       x <= y       |      X =< Y.       |
    +--------------------+--------------------+
    |       x = y        |      x =:= Y.      |
    +--------------------+--------------------+
    |       x != y       |      x =\= Y.      |
    +--------------------+--------------------+
    |       x >= y       |      x <= Y.       |
    +--------------------+--------------------+
    |       x > y        |       X > Y.       |
    +--------------------+--------------------+

* They force both the righthand and lefthand arguments to be evaluated, e.g.:-

    ?- 2 < 4+1.
    true.

* Note that =:= differs from = in that = only tries to unify the left and right-hand sides of
  the operation, whereas =:= forces evaluation. The same goes for \=, e.g.:-

    ?- 3+1 \= 2+2.
    true.

    ?- 3+1 =\= 2+2.
    false.

* Whenever we use these operators, we have to be careful to make sure any variables are
  instantiated. For example:-

    ?- X < 3.
    ERROR: </2: Arguments are not sufficiently instantiated

* Let's put this to work. Let's define a predicate which takes a non-empty list of non-negative
  integers as its first argument, and returns the maximum integer as the last argument. As we
  work our way down the list the accumulator will keep track of the highest integer yet
  encountered.

    accMax([], A, A).
    accMax([H|T], A, M):- H > A,  accMax(T, H, M).
    accMax([_|T], A, M):- H =< A, accMax(T, A, M).

* We have to be careful with this, however, as initialising the accumulator, __A__, to 0 does
  not work when the list contains negative numbers. A way around having to make a choice as to
  the accumulator is to simply use the head of the list, e.g.:-

    max([H|T], M):-
        accMax([H|T],H,M).

* Another way of expressing this program is:-

    max(List, M):-
        List = [H|_],
        accMax(List,H,Max).

* And we also cover negative integers, e.g.:-

    ?- max([-1,-3,-5],M).
    M = -1.

