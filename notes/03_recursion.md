<link href="http://kevinburke.bitbucket.org/markdowncss/markdown.css" rel="stylesheet"></link>

Learn Prolog Now!
=================

Notes for [chapter 3](http://www.learnprolognow.org/lpnpage.php?pagetype=html&pageid=lpn-htmlch3).

I use [swipl](http://www.swi-prolog.org/) as my prolog interpreter.

Chapter 3 - Recursion
---------------------

3.1 Recursive Definitions
-------------------------

* Predicates can be defined recursively. Roughly speaking, a predicate is recursively defined
  if one or more rules in the definition refers to itself.

### Example 1: Eating ###

Consider the following knowledge base:-

    is_digesting(X,Y) :- just_ate(X,Y).
    is_digesting(X,Y) :-
               just_ate(X,Z),
               is_digesting(Z,Y).

    just_ate(mosquito,blood(john)).
    just_ate(frog,mosquito).
    just_ate(stork,frog).

* We can see that __is\_digesting/2__ is recursive, as it appears in both the head and body of
  its rule.

* There is a base case - __just\_ate(X,Y)__.

* Our base case says that if something just ate something, then it is now digesting it.

* The second case of __is\_digesting__ states that if X ate Z, and Z is digesting Y,
  then X is now digesting Y too.

* What is the procedural meaning of this rule?

Consider:-

    ?- is_digesting(stork,mosquito).

* Prolog tries to make use of the first rule, the base rule. This fails, as we have no fact
  which states that the __stork__ just ate a __mosquito__.

* Next, Prolog tries the second rule. __just\_ate(X,Z)__ is unified with
  __just\_ate(stork,\_G1)__, and __is\_digesting(Z,Y)__ is unified with
  __is\_digesting(\_G1,mosquito)__.

* Prolog then looks to fill __\_G1__, which gives us __just\_ate(stork,frog)__, and thus
  __\_G1__ is unified with __frog__.

* Prolog then tries to unify __is\_digesting(frog,mosquito)__, and is able to use the first
  rule to obtain __just\_ate(frog,mosquito)__ and we're done.

* We want to avoid infinite recursion, so rules like:-

    p :- p.

Are not a good idea.

### Example 2: Descendant ###

* Consider the following:-

    child(anne,bridget).
    child(bridget,caroline).
    child(caroline,donna).
    child(donna,emily).

E.g.:- Anne is the mother of Bridget who is the mother of Caroline who is the mother of Donna
who is the mother of Emily.

* We can define this in prolog thus:-

    descend(X,Y) :- child(X,Y).
    descend(X,Y) :- child(X,Z),
        descend(Z,Y).

* Our base case is the first rule - if __Y__ is the child of __X__, then __Y__ is a descendant
  of __X__. This makes sense.

* The second rule is recursive - we say if there exists __Z__ such that __Z__ is the child of
  __X__, and __Y__ is the descendent of __Z__, then __Y__ is the descendent of __X__ too. This
  is the declarative meaning of this predicate.

* Let's look at the procedural meaning of this recursive predicate, by looking at an example:-

    ?- descend(anne,donna)

* Prolog looks at the first rule, and cannot unify __anne__ with __X__ while at the same time
  unifying __Y__ with __donna__.

* Next, Prolog looks at the second rule and repeatedly tries to fit the base rule followed by
  looking at the recursive rule until it is able to unify __X__ and __Y__ with __anne__ and
  __donna__ in the second __descend(X,Y)__ rule. The trace is as follows:-

    [trace]  ?- descend(anne,donna).
       Call: (6) descend(anne, donna) ? creep
       Call: (7) child(anne, donna) ? creep
       Fail: (7) child(anne, donna) ? creep
       Redo: (6) descend(anne, donna) ? creep
       Call: (7) child(anne, _G445) ? creep
       Exit: (7) child(anne, bridget) ? creep
       Call: (7) descend(bridget, donna) ? creep
       Call: (8) child(bridget, donna) ? creep
       Fail: (8) child(bridget, donna) ? creep
       Redo: (7) descend(bridget, donna) ? creep
       Call: (8) child(bridget, _G445) ? creep
       Exit: (8) child(bridget, caroline) ? creep
       Call: (8) descend(caroline, donna) ? creep
       Call: (9) child(caroline, donna) ? creep
       Exit: (9) child(caroline, donna) ? creep
       Exit: (8) descend(caroline, donna) ? creep
       Exit: (7) descend(bridget, donna) ? creep
       Exit: (6) descend(anne, donna) ? creep
    true .

* Here is the search tree for the query:-

<img src="http://www.learnprolognow.org/html/chap3-pspic1.ps.png" />

* Recursion is really important, and most of the work you will do as a Prolog programmer will
  be writing recursive rules.

### Example 3: Successor ###

* Previously we noted that building structure through unification is a key idea in Prolog. Now
  we know about recursion, we can look at more interesting examples of this.

* Let's look at an alternative way of writing numerals which makes use of four symbols - __0__,
  __succ__, __(__ and __)__.

We use the following inductive definition:-

1. __0__ is a numeral.
2. if __X__ is a numeral, then so is __succ(X)__.

* We can easily define this in Prolog:-

    numeral(0).
    numeral(succ(X)) :- numeral(X).

* We can do interesting things with this, e.g.:-

    ?- numeral(X).
    X = 0 ;
    X = succ(0) ;
    X = succ(succ(0)) ;
    X = succ(succ(succ(0))) ;
    X = succ(succ(succ(succ(0)))) ;
    X = succ(succ(succ(succ(succ(0))))) ;
    X = succ(succ(succ(succ(succ(succ(0)))))) ;
    X = succ(succ(succ(succ(succ(succ(succ(0))))))) ;
    X = succ(succ(succ(succ(succ(succ(succ(succ(0)))))))) ;
    X = succ(succ(succ(succ(succ(succ(succ(succ(succ(0))))))))) ;
    X = succ(succ(succ(succ(succ(succ(succ(succ(succ(succ(...)))))))))) .

### Example 4: Addition ###

* Can we use this representation for doing simple arithmetic.

* We want to define a predicate __add/3__ which adds two numerals, can we do this?

* We want to get:-

    ?- add(succ(succ(0)),succ(succ(0)),
        succ(succ(succ(succ(0))))).
    true.

    ?- add(succ(succ(0)),succ(0),Y).
    Y = succ(succ(succ(0))).

There are two important things to note:-

1. Whenever the first argument is 0, the third argument has to be the same as the second
   argument, e.g.:-

    ?- add(0,succ(succ(0)),Y).
    Y = succ(succ(0)).

    ?- add(0,0,Y).
    Y = 0.

This is the case we will use for the base clause.

2. Assume that we want to add the two numerals __X__ and __Y__ (e.g. __succ(succ(succ(0)))__
and __X__ is not 0. Now, if we say that __X1__ is the numeral which has one less __succ__
function than __X__ (e.g. __succ(succ(0))__ in our example), and if we know the result, __Z__,
of adding __X1__ and __Y__, then it's easy to compute the result of adding __X__ and __Y__ -
just add one __succ__ functor to __Z__. We can express this as:-

    add(0,Y,Y).
    add(succ(X),Y,succ(Z)) :-
        add(x,Y,Z).

* Let's look at a trace of an example - __add(succ(succ(succ(0))), succ(succ(0)), R).__ -

    [trace]  ?- add(succ(succ(succ(0))), succ(succ(0)), R).
       Call: (6) add(succ(succ(succ(0))), succ(succ(0)), _G378) ? creep

First of all, we set __\_G378__ to __R__.

       Call: (7) add(succ(succ(0)), succ(succ(0)), _G454) ? creep

Since our first term is not 0 we can't use the base case fact, so we look at the recursive
rule. We unify by stripping a __succ__ from __succ(succ(succ(0)))__, then set __\_G454__ to
__succ(\_G378_)__.

       Call: (8) add(succ(0), succ(succ(0)), _G456) ? creep

We repeat the process, setting __\_G456__ to __succ(\_G454)__.

       Call: (9) add(0, succ(succ(0)), _G458) ? creep

We call again, and this time we hit the base case, with __\_G458__ set to __\_G456__.

       Exit: (9) add(0, succ(succ(0)), succ(succ(0))) ? creep

And we get the result - __\_G458 = succ(succ(0))__.

       Exit: (8) add(succ(0), succ(succ(0)), succ(succ(succ(0)))) ? creep

__\_G456 = succ(succ(succ(0)))__.

       Exit: (7) add(succ(succ(0)), succ(succ(0)), succ(succ(succ(succ(0))))) ? creep

__\_G454 = succ(succ(succ(succ(0))))__.

       Exit: (6) add(succ(succ(succ(0))), succ(succ(0)), succ(succ(succ(succ(succ(0)))))) ? creep
    R = succ(succ(succ(succ(succ(0))))).

__R = \_G378 = succ(succ(succ(succ(succ(0)))))__.

* The search tree:-

<img src="http://www.learnprolognow.org/html/chap3-pspic2.ps.png" />

3.2 Rule Ordering, Goal Ordering, and Termination
-------------------------------------------------

* The idea of logical programming is that the task of the programmer is to simply describe
  problems. The programmer writes down, in the language of logic, a declarative specification
  (i.e. knowledge base) which *describes* the situation of interest, and to get information the
  programmer simply asks questions, the logic programming system figures out how to get the
  answer.

* That's the general idea, and Prolog has taken some important steps in that direction, but it
  is not actually a full logic programming language - if you only think in terms of the
  declarative meaning of a programming language, then you're in for a very tough time.

* Prolog has a very specific way of working out answers to queries - it searches the KB from
  top to bottom, clauses from left to right, and uses backtracking to recover from bad
  choices. These *procedural* aspects have a very important influence on what *actually
  happens* when you make a query. There can be fireworks if things are broken, e.g. __p :- p__.

* We can see how knowledge bases can express the same thing *declaratively*, but behave very
  differently.

Let's look at our previously described descendent program (descend1.prolog):-

    child(anne,bridget).
    child(bridget,caroline).
    child(caroline,donna).
    child(donna,emily).

    descend(X,Y) :- child(X,Y).
    descend(X,Y) :- child(X,Z),
        descend(Z,Y).

Now let's make a single change by reordering the __descend__ rules (descend2.prolog):-

    child(anne,bridget).
    child(bridget,caroline).
    child(caroline,donna).
    child(donna,emily).

    descend(X,Y) :- child(X,Z),
        descend(Z,Y).
    descend(X,Y) :- child(X,Y).

* Actually, nothing has changed declaratively here, but we find a difference in the output of
  queries, e.g. __descend(X,Y)__ returns __X, Y = anne, bridget__ in __descend1.prolog__,
  whereas it returns __X, Y = anne, emily__ in __descend2.prolog__.

* Let's consider yet another change (descend3.prolog):-

    child(anne,bridget).
    child(bridget,caroline).
    child(caroline,donna).
    child(donna,emily).

    descend(X,Y) :- descend(Z,Y),
        child(X,Z).

    descend(X,Y) :- child(X,Y).

* Here we rearrange clauses within a rule. This has a very drastic impact - any attempt at a
  query involving __descend__ will fail with a stack overflow exception, because Prolog will
  attempt to match any __Z__ with a given __Y__, and go off into wonderland.

* This is an example of __left recursion__, where the left-most item of the body is identical
  (except for the choice of variables) to the head. These very often give rise to
  non-terminating computations.

* Let's look at a final example (descend4.prolog):-

    child(anne,bridget).
    child(bridget,caroline).
    child(caroline,donna).
    child(donna,emily).

    descend(X,Y)  :-  child(X,Y).

    descend(X,Y)  :-  descend(Z,Y),
                      child(X,Z).

* We've now placed the base case rule before the recursive rule. This means that true queries
  will succeed, but false queries will fail, e.g. __descend(emily,anne).__

* When it comes to non-terminating programs, changes in rule ordering might result in some
  extra solutions, however goal reordering is more effective, as we've seen left recursion is a
  far bigger problem than in what order results are returned.

* The basic rule of thumb is - avoid left recursion, try to put recursive terms as far right as
  possible in a rule body.
