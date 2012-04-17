<link href="http://kevinburke.bitbucket.org/markdowncss/markdown.css" rel="stylesheet"></link>

Learn Prolog Now!
=================

Notes for
[chapter 10](http://www.learnprolognow.org/lpnpage.php?pagetype=html&pageid=lpn-htmlch10).

I use [swipl](http://www.swi-prolog.org/) as my prolog interpreter.

Chapter 10 - Cuts and Negation
------------------------------

10.1 The Cut
------------

* Automatic backtracking is one of the most characteristic features of Prolog, but it can lead
  to inefficiency.

* So far, we've seen different ways of changing backtracking behaviour - changing the rule
  order and changing the goal order. Both of these are rather crude.

* There is another way - the built-in predicate __!__, called *cut*, which provides a more
  direct way of controlling the way Prolog looks for solutions.

* Cut is essentially a special atom we can use to write clauses.

E.g.:-

    p(X):- b(X), c(X), !, d(X), e(X).

* Cut always succeeds, and has a side effect - if some goal makes use of this clause (the
  *parent goal*), then the cut commits Prolog to any choices that were made since the parent
  *goal was unified with the left hand side of the rule (including the choice of using that
  *particular clause too).

* Let's consider an example.

Firstly, without a cut:-

    p(X):- a(X).
    p(X):- b(X), c(X), d(X), e(X).
    p(X):- f(X).
    a(1).
    b(1).
    b(2).
    c(1).
    c(2).
    d(2).
    e(2).
    f(3).

    ?- p(X).
    X = 1 ;
    X = 2 ;
    X = 3.

The search tree for this knowledge base:-

<img src="http://www.learnprolognow.org/html/chap10-pspic1.ps.png" />

Now let's insert a cut into the second clause:-

    p(X):- b(X), c(X), !, d(X), e(X).

    ?- p(X).
    X = 1 ;
    false.

What's happening?

1. __p(X)__ is unified with the first rule, so we get __a(X)__. By instantiating __X__ to 1,
Prolog unifies __a(X)__ with __a(1)__, and we have a solution. So far, this is no different
from the first version.

2. We then look for a second solution. __p(X)__ is unified with the second rule, so we get the
new goals __b(X)__, __c(X)__, __!__, __d(X)__, __e(X)__. Prolog then unifies __b(X)__ with the
fact __b(1)__, so we now have the goals __c(1)__, __!__, __d(1)__, __e(1)__. We already have
__c(1)__ in the database so this simplifies to __!__, __d(1)__, __e(1)__.

3. Now things change - __!__ succeeds (as it always does), and commits us to the choices made
so far. Particularly, we commit to having __X = 1__, and we're also committed to using the
second rule.

4. __d(1)__ fails. We can't try __X=2__, as __!__ has committed us to our decisions so far, and
nor can we try __X=3__ for the same reason, so there's no way to satisfy __p(X)__.

In terms of the search tree we have:-

<img src="http://www.learnprolognow.org/html/chap10-pspic4.ps.png" />

Search stops when the goal __d(1)__ doesn't lead to any node when an alternative choice is
available. The crosses indicate branches which have been trimmed away.

* An important point to consider is that the cut only commits us to the choices made since the
  parent goal was unified with the left hand side of the clause containing the cut.

E.g., for:-

    q:- p1, ..., pn, !, r1, ..., rm

When we encounter the cut we are committed to using this particular clause for __q__ and the
choices made when evaluating __p1__, ..., __pn__. However, we can backtrack as much as we like
over __r1__, ..., __rm__, and we can also backtrack among choices made before reaching __q__.

* Let's look at a concrete example.

The knowledge base:-

    s(X, Y):- q(X, Y).
    s(0, 0).

    q(X, Y):- i(X), j(Y).

    i(1).
    i(2).

    j(1).
    j(2).
    j(3).

Querying this KB:-

    s(X, Y).
    X = Y, Y = 1 ;
    X = 1,
    Y = 2 ;
    X = 1,
    Y = 3 ;
    X = 2,
    Y = 1 ;
    X = Y, Y = 2 ;
    X = 2,
    Y = 3 ;
    X = Y, Y = 0.

With this corresponding search tree:-

<img src="http://www.learnprolognow.org/html/chap10-pspic5.ps.png" />

* Now let's add a cut.

E.g.:-

    q(X,Y):- i(X), !, j(Y).

This results in:-

    ?- s(X, Y).
    X = Y, Y = 1 ;
    X = 1,
    Y = 2 ;
    X = 1,
    Y = 3 ;
    X = Y, Y = 0.

This is because:-

1. __s(X,Y)__ is first unified with the first rule, which gives us a new goal of __q(X,Y)__.

2. __q(X,Y)__ is then unified with the third rule, so our new goal is __i(X), !, j(Y)__. We
instantiate __X__ to 1, therefore unifying __i(X)__ to __i(1)__. We then end up with the goal
__!,j(Y)__. The cut succeeds (of course), and we are committed to the choices we've made so
far.

3. What are these choices we've committed to? That a. __X=1__, and that we're using this
clause. Note that we haven't chosen a value for __Y__.

4. We instantiate __Y__ to 1 and thus __j(Y)__ unifies to __j(1)__.

5. We then backtrack and obtain __Y=2__, and __Y=3__.

7. These are alternatives for __j(X)__. Backtracking to the left of the cut is not permitted,
so we can't reset __X__ to 2, so we don't find those three solutions that the non-cut program
found. Backtracking over goals which were reached before __q(X,Y)__ *is* allowed, so Prolog
will find the second clause for __s/2__.

And the equivalent search tree is:-

<img src="http://www.learnprolognow.org/html/chap10-pspic6.ps.png" />

10.2 Using Cut
--------------

* So now we know what cut is, but how do we use it in practice, and why is it so useful?

* Let's look at an example of a cut-free predicate __max/3__ which takes integers as arguments
  and succeeds if the third argument is a maximum of the first two.

E.g., we should expect the following queries to succeed:-

    ?- max(2,3,3).
    ?- max(3,2,3).
    ?- max(3,3,3).

And the following queries should fail:-

    ?- max(2,3,2).
    ?- max(2,3,5).

Typically we'd use it thus:-

    ?- max(2,3,Max).
    Max = 3.

We could write this thus:-

    max(X,Y,X):- X >= Y.
    max(X,Y,Y):- Y > X.

* This is a perfectly fine program, only we find that Prolog will attempt to backtrack
  unnecessarily in the case of __Y>X__.

E.g.:-

    ?- max(3,2,X).
    X = 3 ;
    false.

* The two clauses in this program are mutually exclusive. If the first succeeds, the second
  must fail and vice-versa. Attempting to re-satisfy this clause is a waste of time.

* We can fix this with cut.

E.g.:-

    max(X,Y,X):- X >= Y, !.
    max(X,Y,Y):- Y > X.

And we get:-

    ?- max(3,2,X).
    X = 3.

* We might think we can be clever and skip out the second clauses altogether.

E.g.:-

    max(X,Y,X):- X >= Y, !.
    max(X,Y,Y).

However this isn't as clever as it seems. Consider:-

    ?- max(3,2,2).
    true.

This is obviously invalid. What's happened here is that the query has failed to unify at all
with the first clause, but has trivially unified with the second one.

* We don't have this problem with a variable in the third place, as we actually find the
  max in these cases.

* How do we work around this? Our problem was carrying out variable unification before we
  traversed the cut. We can work around this by introducing a third variable, so our first
  clause actually gets evaluated.

E.g.:-

    max(X,Y,Z):- X >= Y, !, Z = X.
    max(X,Y,Y).

This works, e.g.:-

    ?- max(3,2,2).
    false.

* One important thing to note here is that this is a *red* cut, whereas our initial __max__
  program was a *green* cut. A red cut is, by definition, one whose absence changes the meaning
  of the program, while a green cut does not change its meaning.

* Red cuts are dangerous because they make the code ever more less declarative, and more
  imperative, therefore it can be confusing as to how the program is actually going to behave.

* The best approach to writing cuts in code is to get a non-cut program working, and only then
  try to improve its efficiency by using cuts.

10.3 Negation as Failure
------------------------

* One of Prolog's most useful features is the simple means by which it lets us state
  generalisations. If we want to say that Vincent enjoys burgers then we write.

E.g.:-

    enjoys(vincent,X):- burger(X).

* However, in real life we have exceptions. How do we represent this in Prolog? E.g., consider
  the case where Vincent enjoys burgers, but not Big Kahuna burgers. How do we state this in
  Prolog?

* To begin with, let's look at the built-in predicate __fail/0__. This immediately fails when
  Prolog encounters it as a goal.

* Keep in mind that once Prolog fails, it tries to backtrack. As a result, __fail/0__ can be
  considered an instruction to force immediate backtracking. Using this in combination with
  cut, which blocks backtracking, we are able to use this to write some interesting problems,
  particularly defining exceptions to general rules.

Consider the following KB:-

    enjoys(vincent,X):- big_kahuna_burger(X),!,fail.
    enjoys(vincent,X):- burger(X).

    burger(X):- big_mac(X).
    burger(X):- big_kahuna_burger(b).
    burger(X):- whopper(X).

    big_mac(a).
    big_kahuna_burger(b).
    big_mac(c).
    whopper(d).

* From this, we can see that our condition, i.e. that vincent does not like Big Kahuna burgers,
  is applied.

E.g.:-

    ?- enjoys(vincent,a).
    true.

    ?- enjoys(vincent,b).
    false.

    ?- enjoys(vincent,c).
    true.

    ?- enjoys(vincent,d).
    true.

* The reason this works is the combination of __!__ and __fail/0__ - the *cut-fail
  combination*. When we run the query __enjoys(vincent,b)__, the first rule applies, and we
  reach the cut. This commits us to the choices we've made, most importantly of course blocking
  the second rule. When we hit __fail/0__, we are forced to backtrack, but the cut prevents it
  so our query fails.

* There is a problem here - the ordering of the rules is vital. If we reverse the first two
  rules, then the meaning of this program changes. Note also that if we remove the cut, the
  program doesn't work properly - a red cut.

* It'd be better if we could extract this to make the program less reliant on the procedural
  qualities of the program.

* We can abstract this idea away.

E.g.:-

    neg(Goal):- Goal,!,fail.
    neg(Goal).

This will succeed if __Goal__ does not succeed. So we can replace the first rule with:-

    enjoys(vincent,X):- burger(X), neg(big_kahuna_burger(X)).

* Negation as failure is an important tool - it's expressive (we can describe exceptions),
  while being relatively 'safe'. Using this, rather than using the lower-level cut form helps
  us avoid the programming errors that often go along with red cuts.

* In fact, it's so useful that it often comes built in as the __\\+__ operator.

E.g.:-

    enjoys(vincent,X):- burger(X), \+ big_kahuna_burger(X).

* We have to be careful with the order of things again, e.g. if we rewrote our rule.

E.g.:-

    enjoys(vincent,X):- \+ big_kahuna_burger(X), burger(X).

And if we then pose the query, we get:-

    ?- enjoys(vincent,X).
    false.

* This fails because __big\_kahuna\_burger(X)__ holds as Prolog unifies __X__ to b.

Trace:-

    [trace]  ?- enjoys(vincent,X).
       Call: (6) enjoys(vincent, _G560) ? 
       Call: (7) big_kahuna_burger(_G560) ? 
       Exit: (7) big_kahuna_burger(b) ? 
       Fail: (6) enjoys(vincent, _G560) ? 
    false.

* It's generally better to use the *negation as failure* operator than red cuts, but this isn't
  always the case. As always, perf hacks are the exception to the rule.

* Consider the case where we want to capture the condition: 'p holds if a and b hold or a does
  not hold and c holds too.'

We can capture this as:-

    p:- a, b.
    p:- \+ a, c.

* However, suppose this is a very complicated goal which takes a lot of time to compute - we
  might end up having to compute __a__ twice. We can use a red cut to lock into choosing __a__
  and write this as:-

E.g.:-

    p:- a,!,b.
    p:- c.
