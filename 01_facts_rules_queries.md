<link href="http://kevinburke.bitbucket.org/markdowncss/markdown.css" rel="stylesheet"></link>

Learn Prolog Now!
=================

Notes for [chapter 1](http://www.learnprolognow.org/lpnpage.php?pagetype=html&pageid=lpn-htmlch1).

I use [swipl](http://www.swi-prolog.org/) as my prolog interpreter.

Chapter 1 - Facts, Rules and other Queries
------------------------------------------

1.1 Some Simple Examples
------------------------

* Prolog is short for 'programming with logic'.

* There are three basic constructs in Prolog - facts, rules and queries.

* A collection of facts and rules is called a *knowledge base*.

* Prolog is all about writing knowledge bases, and Prolog programs are essentially knowledge bases.

### Knowledge Base 1 ###

* Let's look at an example:-

    woman(mia).
    woman(jody).
    woman(yolanda).
    playsAirGuitar(jody).
    party.

* We'll call this collection of facts KB1. It's our first example of a prolog program.

* This program has names mia, jody and yolanda, and *properties* woman and playsAirGuitar and the
  proposition party.

* Note that all the terms here start with a lower-case letter.

* How can we use KB1? By posing *queries*, i.e. asking questions about the information KB1
  contains. E.g., is Mia a woman?:-

    ?- woman(mia).
    true.

* Note the full stop is important. Without it prolog won't start working on the query.

* Similarly:-

    ?- playsAirGuitar(jody).
    true.
    ?- playsAirGuitar(mia).
    false.

* If we ask about a property we don't know about we receive the following (in swipl):-

    ?- tattooed(jody).
    ERROR: toplevel: Undefined procedure: tattooed/1 (DWIM could not correct goal)

* We can query predicates too:-

    ?- party.
    true.
    ?- rockConcert.
    ERROR: toplevel: Undefined procedure: rockConcert/0 (DWIM could not correct goal)

### Knowledge Base 2 ###

* Let's look at another knowledge base:-

    happy(yolanda).
    listens2Music(mia).
    listens2Music(yolanda):- happy(yolanda).
    playsAirGuitar(mia):- listens2Music(mia).
    playsAirGuitar(yolanda):- listens2Music(yolanda).

* There are 2 facts here, happy and listens2Music. The last 3 items are rules.

* Rules specify information which is conditional, e.g. Yolanda listens to music if she's happy,
  Mia plays air guitar if she's listening to music and Yolanda plays air guitar if she's
  listening to music.

* The ':-' should be read as 'if', or 'is implied by'.

* The part on the left of the ':-' is called the 'head' of the rule, and the part on the right
  is called the 'body' of the rule.

* So in general, if the body of the rule is true, then the head of the rule is true too.

* If a knowledge base contains a rule:-

    head :- body

And Prolog knows that the body follows from the information in the knowledge base, then Prolog
can infer head.

* This fundamental deduction step is called *modus ponens*.

* An example is:-

    ?- playsAirGuitar(mia).
    true.

Here, though we've not explicitly recorded the fact that mia playsAirGuitar, Prolog has
*inferred* that this is the case, as we have specified listens2Music(mia) as a fact.

* Prolog can chain together uses of modus ponens. E.g.:-

    ?- playsAirGuitar(yolanda).
    true.

Here we know happy(yolanda) as a fact, and that listens2Music(yolanda) follows from this, and
that playsAirGuitar(yolanda) follows from listens2Music(yolanda).

* In general:- any fact produced by an application of modus ponens can be used as inputs to
  further rules.

* By chaining together applications of modus ponens in this way, Prolog can retrieve
  information which logically follows from the rules and facts recorded in the knowledge base.

* The happy predicate is defined using a single clause (a fact). The listens2Music and
  playsAirGuitar predicates are formed of two clauses each (comprised of one fact + one rule
  and two rules respectively.)

* We can view a fact as a rule with an empty body - facts can be considered to be conditions
  without antecedent conditions, or *degenerate rules*.

### Knowledge Base 3 ###

* Let's look at the contents of Knowledge Base 3:-

    happy(vincent).
    listens2Music(butch).
    playsAirGuitar(vincent):-
        listens2Music(vincent),
        happy(vincent).
    playsAirGuitar(butch):-
        happy(butch).
    playsAirGuitar(butch):-
        listens2Music(butch).

* There are two facts and three rules here.

* We have the same three predicates as KB2, only we define them differently. Note
  particularly:-

    playsAirGuitar(vincent):-
        listens2Music(vincent),
        happy(vincent).

* This has two *goals* in its body.

* It's important to note the comma n this, it separates the two goals listens2Music(vincent)
  and happy(vincent). This is the way *logical conjunction* (and) is expressed in prolog.

* This rule therefore reads 'Vincent plays air guitar if he listens to music and he is happy'.

* Thus:-

    ?- playsAirGuitar(vincent).
    false.

* Note spacing doesn't matter here.

* Note Butch's predicates are separated into two separate statements. This is equivalent of
  logical disjunction - 'or'.

Note that, due to modus ponens:-

    ?- playsAirGuitar(butch).
    true.

* We can also express logical disjunction as follows:-

    playsAirGuitar(butch):-
        happy(butch);
        listens2Music(butch).

### Knowledge Base 4 ###

* KB4:-

    woman(mia).
    woman(jody).
    woman(yolanda).

    loves(vincent,mia).
    loves(mersellus,mia).
    loves(pumpkin,honey_bunny).
    loves(honey_bunny,pumpkin).

* This is just a collection of facts.

* We have relations between 2 names, however this is hardly anything altogether novel.

* We do introduce a novel query:-

    ?- woman(X).
    X = mia .

* The X is a variable.

* Any word beginning with an uppercase letter in prolog is a variable.

* A variable isn't a name, it's a placeholder for information.

* The query above essentially asks Prolog which of the individuals it knows about is a woman.

* Prolog answers this query by working through KB4, from top to bottom, attempting to *unify*
  (match) the expression woman(X) with the information KB4 contains.

* The first item in the knowledge base is woman(mia), so Prolog unifies X with mia.

* We could also say Prolog *instantiates* X to mia, or *binds* X to mia rather than unify.

* We can do better than this - the whole point of variables is that they can stand for, or
  unify with, different things. There is information about other women in the knowledge base
  which we could potentially obtain here. We can obtain these using the logical disjunction
  operator ';':-

    ?- woman(X).
    X = mia ;
    X = jody ;
    X = yolanda.

Here we're hitting ; each time a result is returned by swipl.

* Let's try something more complicated:-

    ?- loves(marsellus,X), woman(X).
    X = mia.

* Prolog is very interesting, but at core the most important aspect of it is its ability to
  perform unification and return the values of variable bindings.

### Knowledge Base 5 ###

* We've see the use of variables in queries, however we can also use them in knowledge
  bases. E.g. KB5:-

    loves(vincent, mia).
    loves(marsellus, mia).
    loves(pumpkin, honey_bunny).
    loves(honey_bunny, pumpkin).

    jealous(X, Y):- loves(X, Z), loves(Y, Z).

* The jealousy rule defines the rule that X will be jealous of Y if both X and Y loves Z.

* What's interesting about this rule is that it's stated in general terms - X, Y and Z can bind
  to anyone, and is not limited to specific people.

* We can ask the following query:-

    ?- jealous(marsellus, W).
    W = vincent ;
    W = marsellus.

1.2 Prolog syntax
-----------------

* We've seen lots of examples, but they have just been examples. We need to get precise - what
  exactly are facts, rules and queries built out of?

* The answer is - *terms*. There are four kinds of terms in Prolog:- atoms, numbers, variables
  and complex terms (or structures).

* Atoms and numbers are considered to be *constants*.

* Constants and variables make up the *simple terms* of Prolog.

* An *atom* is either an identifier:-

    [a-z][a-zA-Z0-9_]*

Or an arbitrary sequence of characters enclosed in single quotes (the sequence between the
quotes is called the 'atom name').

Or a string of special characters, e.g. @=, ====>, :-. Some of these atoms have a predefined
meaning e.g. ';', ':-'.

* A *number* is as you'd expect.

* A *variable* is:-

    [A-Z][a-zA-Z0-9_]*

* The _ variable, the anonymous variable, is special. We discuss it later.

* *Complex terms* or *structures* are built out of a *functor* followed by a sequence of
   arguments. We've seen this already, e.g. listens2Music(mia).

* You can't have whitespace between the functor and its arguments.

* The functor must be an atom. Arguments can be any term.

* We can just keep on nesting, e.g.:-

    hide(X, father(father(father(butch))))

Here the functor is hide, and it has two arguments: the variable __X__, and the complex term
__father(father(father(butch)))__.

* The number of arguments that a complex term has is called its *arity*, e.g. __woman(mia)__ has
  arity 1, and __loves(vincent, mia)__ has arity 2.

* We could, for example, define two predicates for love like __love(vincent, mia)__ and
  __love(vincent, marsellus, mia)__. Prolog considers these to be different predicates.

* It's usual to refer to predicates with a suffix /N where N is the predicate's arity. E.g.,
  from KB2:-

    listens2Music/1
    happy/1
    playsAirGuitar/1

* Can obtain a listing of code using the following command:-

    listing.

* Can get all listing relating to a predicate by using a command like:-

    listing(listens2Music).

2.1 Unification
---------------

* We briefly mentioned unification earlier (in relation to KB4, e.g. woman(X) unifying with
  woman(mia), thereby instantiating the variable X with mia). Let's look a bit closer.

* There are three types of term:-

    constants - atoms/numbers
    variables
    complex terms

* Let's work our way towards a definition of when Prolog will unify two terms. A basic
  definition is as follows:-

>Two terms unify if they are the same term or if they contain variables that can be uniformly
>instantiated with terms in such a way that the resulting terms are equal.

* So the terms __mia__ and __mia__ unify, because they are the same atom.

* The terms 42 and 42 unify, because they are the same number.

* The terms __X__ and __X__ unify, because they are the same variable.

* The terms __woman(mia)__ and __woman(mia)__ unify, because they are the same complex term.

* The terms __woman(mia)__ and __woman(vincent)__ do not unify, because they are not the same,
  and neither of them contain a variable which could be instantiated to make them the same.

* What about the terms __mia__ and __X__? They are not the same, however __X__ can be
  instantiated to __mia__ which makes them equal. So by the second part of our working
  definition,  __mia__ and __X__ unify.

* Similarly, the terms __woman(X)__ and __Woman(mia)__ unify, because they can be made equal by
  instantiating __X__ to __mia__.

* How about __loves(vincent,X)__ and __loves(X,mia)__? No. It is impossible to find an
  instantiation of __X__ which makes the two terms equal, as if we instantiated __X__ to
  __vincent__ then we'd end up with __loves(vincent,vincent)__ and __loves(vincent,mia)__ which
  are not equal, and if we instantiated __X__ to __mia__ then we'd end up with
  __loves(vincent,mia)__ and __loves(mia,mia)__ which are also not equal.

* Typically, we're not only interested in the fact that two terms unify, we're also (and
  primarily) interested in how the variables have to be instantiated to make them equal. Prolog
  provides us with this information - it makes all the necessary instantiations, so that the
  terms really are equal afterwards.

### Precise Definition of Unification ###

Let's look at a more precise definition of unification:-

<br />

1. If __term1__ and __term2__ are constants, then __term1__ and __term2__ unify if and only if
they are the same atom, or the same number.

2. If __term1__ is a variable, and __term2__ is any type of term, then __term1__ and __term2__
unify, and __term1__ is instantiated to __term2__. Similarly, if __term2__ is a variable, and
__term1__ is any type of term, then __term1__ and __term2__ unify, and __term2__ is
instantiated to __term1__ (if they are both variables, they're both instantiated to each other,
and we say they share values.)

3. If __term1__ and __term2__ are complex terms, then they unify if and only if they have the
same functor and arity, and all their corresponding arguments unify, and the variable
instantiations are compatible (for example, it is not possible to instantiate variable X to mia
when unifying one pair of arguments, and to instantiate __X__ to __vincent__ when unifying
another pair of arguments.)

4. Two terms unify if and only if it follows from the previous three points that they unify.

<br />

* Note the structure of this definition. The first three clauses mirror perfectly the
  (recursive) structure of terms.

* The first point tells us when two constants unify.

* The second tells us when two terms, one of which is a variable, unify (such terms will always
  unify, variables unify with anything). Just as importantly, this point also tells us what
  instantiations we have to perform to make the two terms the same.

* The third point tells us when two complex terms unify.

* The fourth point is also important - it tells us that the previous 3 points tell us all we
  need to know about unification of two terms. If two terms can't be shown to unify using
  clauses 1-3, then they don't unify.

### Examples ###

To test understanding, let's work through several examples.

* The __=/2__ predicate determines whether two arguments unify, e.g.:-

    ?- =(mia,mia).
    true.
    ?- =(mia,vincent).
    false.

* We can also do this inline, e.g.:-

    ?- mia = mia.
    true.

* This is true from point 1. Two constants unify if they are exactly the same.

* Let's try an example with a variable:-

    ?- mia = X.
    X = mia.

* Clearly __X__ can be unified with __mia__, however how does this follow from our definition?
  It comes from point 2.

* Consider the following query:-

    ?- X = Y.
    X = Y.

* Again this is point 2 at play. These can clearly unify with any term, most certainly another
  variable. Prolog simply notes that __X__ and __Y__ denote the same object.

* Let's consider:-

    ?- X = mia, X = vincent.
    false.

* Taken separately, Prolog would succeed at both of them, instantiating __X__ to __mia__ in the
  first instance, and __X__ to __vincent__ in the second, however we are using the logical
  conjunction operator, so expect both of these to be true at once. The problem is that __X__
  is matched to __mia__, and then it is unable to match to __vincent__.

* This leads to a general point - an instantiated variable really isn't a variable anymore, it
  becomes what it was instantiated with.

* Let's look at an example involving complex terms:-

    ?- k(s(g),Y) = k(X,t(k)).
    Y = t(k),
    X = s(fg).

* Let's look at another:-

    ?- loves(X,X) = loves(marcellus,mia).
    false.

* Here we violate a part of the 3rd point, namely that the variable instantiations have to be
  compatible. __X=marcellus__, and __X=mia__ are not compatible as X cannot be both at once. If
  we unified __X__ with __marcellus__ then __X__ cannot be __mia__, however if we unified __X__
  with __mia__ then __X__ cannot be __marcellus__.

### The Occurs Check ###

* Unification is a well-known concept and used in many areas of computer science - many
  unification algorithms are known. However, note that Prolog doesn't use a standard version of
  unification when it performs its version of it, rather it takes a shortcut. We need to take a
  look at this shortcut.

* Consider the following:-

    ?- father(X) = X.

* Do these terms unify or not? A standard unification algorithm would say not. Why? Because of
  point 2, if we set __X = father(X)__, We'd then realise that the other side of the
  equation is now __father(father(X))__, so we have to set __X__ to __father(father(X))__ to
  try to compensate. If we keep on going with this we end up with an infinite recursion, since
  the term on the right will always be one symbol longer than the terms on the left.

* Strangely enough, __swipl__ doesn't appear to actually result in either a crash or an
  indication that there is an infinite regress here, instead giving:-

    ?- father(X)=X.
    X = father(X).

* A standard algorithm, given two terms to unify, will first carry out what is known as the
  'occurs check'. This means that when it is asked to unify a variable with a term, it first
  checks to see whether that variable occurs in the term. If so, it reports that the operation
  is impossible. This is a 'pessimistic' approach.

* Prolog, on the other hand, is optimistic. As a perf hack, we don't check for occurs.

* Prolog does have a predicate which allows us to perform unification *with* an occurs check,
  unify\_with\_occurs\_check/2, e.g.:-

    unify_with_occurs_check(father(X), X).
    false.

### Programming with Unification ###

* Unification is a fundamental operation in Prolog. It plays an important role in Prolog proof
  search (as we shall see), which alone makes it very useful. However, it can be useful just to
  have complex terms, through which programs can be defined by these terms alone, e.g.:-

    vertical(line(point(X,Y),point(X,Z))).
    horizontal(line(point(X,Y),point(Z,Y))).

* Here we encode the fact that two points with the same __X__ coordinate are vertical, and two
  points with the same __Y__ coordinate are horizontal.

* We can perform some queries on this:-

    ?- vertical(line(point(1,1),point(1,3))).
    true.

    ?- vertical(line(point(1,1),point(3,2))).
    false.

* Note that what's really powerful is that we can ask more general questions, e.g.:-

    ?- horizontal(line(point(1,1),point(2,Y))).
    Y = 1.

* And also:-

    ?- horizontal(line(point(2,3),P)).
    P = point(_G307, 3).

(Note that __\_G307__ is a general variable which means any X is permissible.)

* Note that the answer given to this query is structured. No logical inference was required (no
  modus ponens).

* This style of programming is particularly useful where the important concepts have natural
  hierarchical structure, where we can use complex terms to represent this structure, and
  unification to access it.

2.2 Proof Search
----------------

* Now that we know about unification, we are now in a position to learn how Prolog *actually*
  searches a knowledge base to see if a query is satisfied, i.e. we're ready to learn about
  proof search.

* Consider the following knowledge base:-

    f(a).
    f(b).

    g(a).
    g(b).

    h(b).

    k(X) :- f(X), g(X), h(X).

* Suppose we posed the following query:-

    ?- k(Y).

* It is clear that the answer to the query ought to be __Y = b__, but how does Prolog work this
  out?

* Prolog reads the knowledge base and tries to unify __k(Y)__ with either a fact, or the head
  of a rule.

* Clearly, the only thing Prolog can now unify with is our __k(X)__ rule. It does this by
  generating a brand new variable (e.g. __\_G34__) to represent the shared variables. So the
  original query now reads:-

    k(_G34)

* And Prolog knows that:-

    k(_G34) :- f(_G34), g(_G34), h(_G34).

* Prolog replaces the original query with the following list of goals:-

    f(_G34), g(_G34), h(_G34).

* We can represent this graphically:-

<img src="http://www.learnprolognow.org/html/chap2-pspic1.ps.png" />

Everything in a box is either a query or a goal.

* Whenever Prolog has a list of goals, it tries to satisfy them one by one, working through the
  list left-to-right.

* The leftmost goal is __f(\_G34)__, which reads: 'I want an individual with property f'. Can we
  satisfy this? Prolog tries to by searching through the knowledge base from top to bottom.

* The first item Prolog finds which unifies with this goal is __f(a)__. This satisfies
  __f(\_G34)__ and we are left with two remaining goals. Our goals now look like:-

    g(a), h(a)

* And our graphical representation now looks like:-

<img src="http://www.learnprolognow.org/html/chap2-pspic2.ps.png" />

* The knowledge base contains an entry __g(a)__, so our goal list now becomes:-

    h(a).

* And our graphical representation now looks like:-

<img src="http://www.learnprolognow.org/html/chap2-pspic3.ps.png" />

* There is no way to satisfy __h(a)__, the last remaining goal. The only information about
  __h__ we have in the knowledge base is __h(b)__, and that won't unify with __h(a)__.

* So what does Prolog do now? It decides it's made a mistake, and checks whether it has missed
  any of the possible ways of unifying a goal with a fact or the head of a rule in the
  knowledge base.

* Prolog does this by going back up the path shown in the graphical representation, looking for
  alternatives.

* There is nothing else in the knowledge base which unifies with __g(a)__, but there is another
  way of unifying __(f\_G34)__.

* Points in the search where there are several alternative ways of unifying a goal against the
  knowledge base are called 'choice points'. Prolog keeps a track of these as it goes, so if a
  wrong choice is made it can retreat to the previous choice point and try something else
  instead. This process is called *backtracking*, and is fundamental to proof search in Prolog.

* So, Prolog backtracks to the previous choice point, this is the point where the list of goals
  was:-

    f(_G34),g(_G34),h(_G34).

* Prolog must now redo this work. First it must try to re-satisfy the first goal by searching
  further in the knowledge base. It turns out that it can do it by unifying __f(\_G34)__ with
  __f(b)__. This satisfies __f(\_G34)__ and instantiates __X__ to __b__, so the remaining goal
  list is:-

    g(b),h(b).

* __g(b)__ is in the knowledge base, so this is satisfied too, leaving the goal list:-

    h(b).

* __h(b)__ is also in the knowledge base, so this is also satisfied.

* Prolog now has an empty list of goals. This means it has now proved everything required to
  establish the original goal (__k(Y)__), so the original query is satisfiable, and Prolog has
  determined what to do to satisfy it (instantiate __Y__ to __b__).

* Had there been more than one possible means of satisfying the problem, then we could have
  made Prolog backtrack by prodding it with ;'s.

* Let's consider a graphical representation of the whole process:-

<img src="http://www.learnprolognow.org/html/chap2-pspic4.ps.png" />

* This diagram is in the form of a tree, and is in fact our first example of what is known as a
  search tree.

* The nodes of these trees say which goals have to be satisfied at the various stages of the
  proof search, and edges keep track of the variable instantiations which are made when the
  current goal (e.g. the first one in the list of goals) is unified to a fact or to the head of
  a rule in the knowledge base.

* Leaf nodes which still contain unsatisfied goals are points where Prolog failed (either
  because it made a wrong decision somewhere along the path, or because no solution exists.)

* Leaf nodes with an empty goal list correspond to a possible solution.

* The edges along a path from the root node to a successful leaf node tells you the variable
  instantiations that need to be made to satisfy the original query.

* Let's look at another example, using the following familiar knowledge base:-

    loves(vincent,mia).
    loves(marcellus,mia).

    jealous(A,B):- loves(A,C), loves(B,C).

* If we now pose the following query:-

    ?- jealous(X,Y).

* Our search tree looks like:-

<img src="http://www.learnprolognow.org/html/chap2-pspic5.ps.png" />

* There's only one possible way of unifying __jealous(X,Y)__ against the knowledge base, namely
  by using the rule:-

    jealous(A,B):- loves(A,C), loves(B,C).

* Meaning the new goals that have to be satisfied are:-

    loves(_G5,_G6),loves(_G7,_G6).

* There are two ways of unifying __loves(\_G5,\_G6)__ against the knowledge base:-

All-in-all there are four leaf nodes with an empty goal list, which means there are four
  solutions to the problem:-

1. X = _G5 = vincent, Y = _G7 = vincent.
2. X = _G5 = vincent, Y = _G7 = marcellus.
3. X = _G5 = marcellus, Y = _G7 = vincent.
4. X = _G5 = marcellus, Y = _G7 = marcellus.
