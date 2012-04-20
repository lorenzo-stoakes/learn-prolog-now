<link href="http://kevinburke.bitbucket.org/markdowncss/markdown.css" rel="stylesheet"></link>
 
Learn Prolog Now!
=================

Notes for [chapter 1](http://www.learnprolognow.org/lpnpage.php?pagetype=html&pageid=lpn-htmlch1).

I use [swipl](http://www.swi-prolog.org/) as my prolog interpreter.

Chapter 1 - Facts, Rules and Other Queries
------------------------------------------

1.1 Some Simple Examples
------------------------

* Prolog is short for 'programming with logic'.

* There are three basic constructs in Prolog - facts, rules and queries.

* A collection of facts and rules is called a *knowledge base*.

* Prolog is all about writing knowledge bases, and Prolog programs are essentially knowledge bases.

### Knowledge Base 1 ###

* Let's look at an example:-

```prolog
    woman(mia).
    woman(jody).
    woman(yolanda).
    playsAirGuitar(jody).
    party.
```

* We'll call this collection of facts KB1. It's our first example of a prolog program.

* This program has names __mia__, __jody__ and __yolanda__, and *properties* __woman__ and
  __playsAirGuitar__ and the proposition __party__.

* Note that all the terms here start with a lower-case letter.

* How can we use KB1? By posing *queries*, i.e. asking questions about the information KB1
  contains. E.g., is Mia a woman?:-

```prolog
    ?- woman(mia).
    true.
```

* Note the full stop is important. Without it prolog won't start working on the query.

* Similarly:-

```prolog
    ?- playsAirGuitar(jody).
    true.
    ?- playsAirGuitar(mia).
    false.
```

* If we ask about a property we don't know about we receive the following (in swipl):-

```prolog
    ?- tattooed(jody).
    ERROR: toplevel: Undefined procedure: tattooed/1 (DWIM could not correct goal)
```

* We can query predicates too:-

```prolog
    ?- party.
    true.
    ?- rockConcert.
    ERROR: toplevel: Undefined procedure: rockConcert/0 (DWIM could not correct goal)
```

### Knowledge Base 2 ###

* Let's look at another knowledge base:-

```prolog
    happy(yolanda).
    listens2Music(mia).
    listens2Music(yolanda):- happy(yolanda).
    playsAirGuitar(mia):- listens2Music(mia).
    playsAirGuitar(yolanda):- listens2Music(yolanda).
```

* There are 2 facts here, happy and listens2Music. The last 3 items are rules.

* Rules specify information which is conditional, e.g. Yolanda listens to music if she's happy,
  Mia plays air guitar if she's listening to music and Yolanda plays air guitar if she's
  listening to music.

* The __:-__ should be read as *if*, or *is implied by*.

* The part on the left of the __:-__ is called the *head* of the rule, and the part on the right
  is called the *body* of the rule.

* So in general, if the body of the rule is true, then the head of the rule is true too.

* If a knowledge base contains a rule:-

```prolog
    head :- body
```

And Prolog knows that the body follows from the information in the knowledge base, then Prolog
can infer head.

* This fundamental deduction step is called *modus ponens*.

* An example is:-

```prolog
    ?- playsAirGuitar(mia).
    true.
```

Here, though we've not explicitly recorded thef act that __mia__ __playsAirGuitar__, Prolog has
*inferred* that this is the case, as we have specified __listens2Music(mia)__ as a fact.

* Prolog can chain together uses of modus ponens:-

```prolog
    ?- playsAirGuitar(yolanda).
    true.
```

Here we know __happy(yolanda)__ as a fact, and that __listens2Music(yolanda)__ follows from
this, and that __playsAirGuitar(yolanda)__ follows from __listens2Music(yolanda)__.

* In general:- any fact produced by an application of modus ponens can be used as inputs to
  further rules.

* By chaining together applications of modus ponens in this way, Prolog can retrieve
  information which logically follows from the rules and facts recorded in the knowledge base.

* The __happy__ predicate is defined using a single *clause* (a fact). The __listens2Music__
  and __playsAirGuitar__ *predicates* are formed of two clauses each (comprised of one fact +
  one rule and two rules respectively.)

* We can view a fact as a rule with an empty body - facts can be considered to be conditions
  without antecedent conditions, or *degenerate* rules.

### Knowledge Base 3 ###

* Let's look at the contents of Knowledge Base 3:-

```prolog
    happy(vincent).
    listens2Music(butch).
    playsAirGuitar(vincent):-
        listens2Music(vincent),
        happy(vincent).
    playsAirGuitar(butch):-
        happy(butch).
    playsAirGuitar(butch):-
        listens2Music(butch).
```

* There are two facts and three rules here.

* We have the same three predicates as KB2, only we define them differently. Note
  particularly:-

```prolog
    playsAirGuitar(vincent):-
        listens2Music(vincent),
        happy(vincent).
```

* This has two *goals* in its body.

* It's important to note the comma in this, it separates the two goals
  __listens2Music(vincent)__ and __happy(vincent)__. This is the way *logical conjunction*
  (logical and) is expressed in prolog.

* This rule therefore reads 'Vincent plays air guitar if he listens to music *and* he is
  happy'.

* Thus:-

```prolog
    ?- playsAirGuitar(vincent).
    false.
```

* Note spacing doesn't matter here.

* Note Butch's predicates are separated into two separate statements. This is equivalent of
  *logical disjunction* (logical or).

Note that, due to modus ponens:-

```prolog
    ?- playsAirGuitar(butch).
    true.
```

* We can also express logical disjunction as follows:-

```prolog
    playsAirGuitar(butch):-
        happy(butch);
        listens2Music(butch).
```

### Knowledge Base 4 ###

* KB4:-

```prolog
    woman(mia).
    woman(jody).
    woman(yolanda).

    loves(vincent,mia).
    loves(mersellus,mia).
    loves(pumpkin,honey_bunny).
    loves(honey_bunny,pumpkin).
```

* This is just a collection of facts.

* We have relations between 2 names, however this is hardly anything altogether novel.

* We do introduce a novel query:-

```prolog
    ?- woman(X).
    X = mia .
```

* The __X__ is a variable.

* Any word beginning with an uppercase letter in prolog is a variable.

* A variable isn't a name, it's a placeholder for information.

* The query above essentially asks Prolog which of the individuals it knows about is a woman.

* Prolog answers this query by working through KB4, from top to bottom, attempting to *unify*
  (match) the expression woman(X) with the information KB4 contains.

* The first item in the knowledge base is __woman(mia)__, so Prolog unifies __X__ with __mia__.

* We could also say Prolog *instantiates* X to mia, or *binds* X to mia rather than unify.

* We can do better than this - the whole point of variables is that they can stand for, or
  unify with, different things. There is information about other women in the knowledge base
  which we could potentially obtain here. We can obtain these using the logical disjunction
  operator __;__:-

```prolog
    ?- woman(X).
    X = mia ;
    X = jody ;
    X = yolanda.
```

Here we're typing __;__ each time a result is returned by swipl.

* Let's try something more complicated:-

```prolog
    ?- loves(marsellus,X), woman(X).
    X = mia.
```

* At the core of Prolog is its ability to perform unification and return the values of variable
  bindings.

### Knowledge Base 5 ###

* We've see the use of variables in queries, however we can also use them in knowledge
  bases. E.g. KB5:-

```prolog
    loves(vincent, mia).
    loves(marsellus, mia).
    loves(pumpkin, honey_bunny).
    loves(honey_bunny, pumpkin).

    jealous(X, Y):- loves(X, Z), loves(Y, Z).
```

* The jealousy rule defines the rule that __X__ will be jealous of __Y__ if both __X__ and
  __Y__ love __Z__.

* What's interesting about this rule is that it's stated in general terms - __X__, __Y__ and
  __Z__ can bind to anyone, and is not limited to specific people.

* We can ask the following query:-

```prolog
    ?- jealous(marsellus, W).
    W = vincent ;
    W = marsellus.
```

1.2 Prolog syntax
-----------------

* We've seen lots of examples, but they have just been examples. We need to get precise - what
  exactly are facts, rules and queries built out of?

* The answer is - *terms*. There are four kinds of terms in Prolog:- atoms, numbers, variables
  and complex terms (or structures).

* Atoms and numbers are *constants*.

* Constants and variables make up the *simple terms* of Prolog.

* An *atom* is either an identifier:-

```
    [a-z][a-zA-Z0-9_]*
```

Or an arbitrary sequence of characters enclosed in single quotes (the sequence between the
quotes is called the 'atom name').

Or a string of special characters, e.g. __@=__, __====>__, __:-__. Some of these atoms have a
predefined meaning e.g. __;__, __:-__.

* A *number* is as you'd expect.

* A *variable* is:-

```
    [A-Z][a-zA-Z0-9_]*
```

* The _____ variable, the anonymous variable, is special. We discuss it later.

* *Complex terms* or *structures* are built out of a *functor* followed by a sequence of
   arguments. We've seen this already, e.g. __listens2Music(mia)__.

* You can't have whitespace between the functor and its arguments.

* The functor must be an atom. Arguments can be any term.

* We can just keep on nesting.

```prolog
    hide(X, father(father(father(butch))))
```

Here the functor is hide, and it has two arguments: the variable __X__, and the complex term
__father(father(father(butch)))__.

* The number of arguments that a complex term has is called its *arity*, e.g. __woman(mia)__ has
  arity 1, and __loves(vincent, mia)__ has arity 2.

* We could, for example, define two predicates for love like __love(vincent, mia)__ and
  __love(vincent, marsellus, mia)__. Prolog considers these to be different predicates.

* It's usual to refer to predicates with a suffix /N where N is the predicate's arity. E.g.,
  from KB2:-

```prolog
    listens2Music/1
    happy/1
    playsAirGuitar/1
```

* Can obtain a listing of code using the following command:-

```prolog
    listing.
```

* Can get all listing relating to a predicate by using a command like:-

```prolog
    listing(listens2Music).
```
