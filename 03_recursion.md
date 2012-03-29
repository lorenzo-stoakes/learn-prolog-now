<link href="http://kevinburke.bitbucket.org/markdowncss/markdown.css" rel="stylesheet"></link>

Learn Prolog Now!
=================

Notes for [chapter 2](http://www.learnprolognow.org/lpnpage.php?pagetype=html&pageid=lpn-htmlch3).

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

