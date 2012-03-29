<link href="http://kevinburke.bitbucket.org/markdowncss/markdown.css" rel="stylesheet"></link>

Learn Prolog Now!
=================

Exercises/Chapter 2 - Unification and Proof Search
--------------------------------------------------

2.3 Exercises
-------------

### 2.1 ###

Which of the following pairs of terms unify? Where relevant, give the variable instantiations
that lead to successful unification.

1.   bread = bread

    true.

2.   'Bread' = bread

    false.

3.   'bread' = bread

    true.

4.   Bread = bread

    true.
    Bread = bread.

5.   bread = sausage

    false.

6.   food(bread) = bread

    false.

7.   food(bread) = X

    true.
    X = food(bread).

8.   food(X) = food(bread)

    true.
    X = bread.

9.   food(bread,X) = food(Y,sausage)

    true.
    X = sausage.    
    Y = bread.

10.  food(bread,X,beer) = food(Y,sausage,X)

    false.
    X can't be both sausage and beer at the same time.

11.  food(bread,X,beer) = food(Y,kahuna_burger)

    false.
    arities don't match.

12.  food(X) = X

    false.
    infinite recursion.

13.  meal(food(bread),drink(beer)) = meal(X,Y)

    true.
    X = food(bread).
    Y = drink(beer).

14.  meal(food(bread),X) = meal(X,drink(beer))

    false.
    X can't be both food(bread) and drink(beer) at once.

### 2.2 ###

We are working with the following knowledge base:-

    house_elf(dobby). 
    witch(hermione). 
    witch(’McGonagall’). 
    witch(rita_skeeter). 
    magic(X):-  house_elf(X). 
    magic(X):-  wizard(X). 
    magic(X):-  witch(X).

Which of the following queries are satisfied? Where relevant, give all the variable
instantiations that lead to success.

1. ?- magic(house_elf).

    false.
    house_elf is a functor.

    magic(dobby) would be true!

2. ?- wizard(harry).

    false.
    We don't know anything about Harry, and no fact refers to wizard.

3. ?- magic(wizard).

    false.

4. ?- magic('McGonagall').

    true.
    witch('McGonagall'). and magic(X):- witch(X). combine.

Actually false - due to wizard() reference:-

    ERROR: magic/1: Undefined procedure: wizard/1

5. ?- magic(Hermione).

    true.
    Hermione = dobby.

Draw the search tree for the query __magic(Hermione).__

<img src="http://codegrunt.co.uk/images/prolog/ex-2-1.jpg" />

### 2.3 ###

Here is a tiny lexicon (information about individual words), and a mini grammar consisting of
one syntactic rule (which defines a sentence to be an entity consisting of five words in the
following order: a determiner, a noun, a verb, a determiner, a noun):-

    word(determiner,a).
    word(determiner,every).
    word(noun,criminal).
    word(noun,'big kahuna burger').
    word(verb,eats).
    word(verb,likes).

    sentence(Word1,Word2,Word3,Word4,Word5):-
        word(determiner,Word1),
        word(noun,Word2),
        word(verb,Word3),
        word(determiner,Word4),
        word(noun,Word5).

What query do you have to pose in order to find out which sentences the grammar can generate?
List all sentences that this grammar can generate in the order that Prolog will generate them
in.

    ?- sentence(W1,W2,W3,W4,W5).
    W1 = a,
    W2 = criminal,
    W3 = eats,
    W4 = a,
    W5 = criminal ;
    W1 = a,
    W2 = criminal,
    W3 = eats,
    W4 = a,
    W5 = 'big kahuna burger' ;
    W1 = a,
    W2 = criminal,
    W3 = eats,
    W4 = every,
    W5 = criminal ;
    W1 = a,
    W2 = criminal,
    W3 = eats,
    W4 = every,
    W5 = 'big kahuna burger' ;
    W1 = a,
    W2 = criminal,
    W3 = likes,
    W4 = a,
    W5 = criminal ;
    W1 = a,
    W2 = criminal,
    W3 = likes,
    W4 = a,
    W5 = 'big kahuna burger' ;
    W1 = a,
    W2 = criminal,
    W3 = likes,
    W4 = every,
    W5 = criminal ;
    W1 = a,
    W2 = criminal,
    W3 = likes,
    W4 = every,
    W5 = 'big kahuna burger' ;
    W1 = a,
    W2 = 'big kahuna burger',
    W3 = eats,
    W4 = a,
    W5 = criminal ;
    W1 = a,
    W2 = 'big kahuna burger',
    W3 = eats,
    W4 = a,
    W5 = 'big kahuna burger' ;
    W1 = a,
    W2 = 'big kahuna burger',
    W3 = eats,
    W4 = every,
    W5 = criminal ;
    W1 = a,
    W2 = 'big kahuna burger',
    W3 = eats,
    W4 = every,
    W5 = 'big kahuna burger' ;
    W1 = a,
    W2 = 'big kahuna burger',
    W3 = likes,
    W4 = a,
    W5 = criminal ;
    W1 = a,
    W2 = 'big kahuna burger',
    W3 = likes,
    W4 = a,
    W5 = 'big kahuna burger' ;
    W1 = a,
    W2 = 'big kahuna burger',
    W3 = likes,
    W4 = every,
    W5 = criminal ;
    W1 = a,
    W2 = 'big kahuna burger',
    W3 = likes,
    W4 = every,
    W5 = 'big kahuna burger' ;
    W1 = every,
    W2 = criminal,
    W3 = eats,
    W4 = a,
    W5 = criminal ;
    W1 = every,
    W2 = criminal,
    W3 = eats,
    W4 = a,
    W5 = 'big kahuna burger' ;
    W1 = every,
    W2 = criminal,
    W3 = eats,
    W4 = every,
    W5 = criminal ;
    W1 = every,
    W2 = criminal,
    W3 = eats,
    W4 = every,
    W5 = 'big kahuna burger' ;
    W1 = every,
    W2 = criminal,
    W3 = likes,
    W4 = a,
    W5 = criminal ;
    W1 = every,
    W2 = criminal,
    W3 = likes,
    W4 = a,
    W5 = 'big kahuna burger' ;
    W1 = every,
    W2 = criminal,
    W3 = likes,
    W4 = every,
    W5 = criminal ;
    W1 = every,
    W2 = criminal,
    W3 = likes,
    W4 = every,
    W5 = 'big kahuna burger' ;
    W1 = every,
    W2 = 'big kahuna burger',
    W3 = eats,
    W4 = a,
    W5 = criminal ;
    W1 = every,
    W2 = 'big kahuna burger',
    W3 = eats,
    W4 = a,
    W5 = 'big kahuna burger' ;
    W1 = every,
    W2 = 'big kahuna burger',
    W3 = eats,
    W4 = every,
    W5 = criminal ;
    W1 = every,
    W2 = 'big kahuna burger',
    W3 = eats,
    W4 = every,
    W5 = 'big kahuna burger' ;
    W1 = every,
    W2 = 'big kahuna burger',
    W3 = likes,
    W4 = a,
    W5 = criminal ;
    W1 = every,
    W2 = 'big kahuna burger',
    W3 = likes,
    W4 = a,
    W5 = 'big kahuna burger' ;
    W1 = every,
    W2 = 'big kahuna burger',
    W3 = likes,
    W4 = every,
    W5 = criminal ;
    W1 = every,
    W2 = 'big kahuna burger',
    W3 = likes,
    W4 = every,
    W5 = 'big kahuna burger'.

### 2.4 ###

Here are six italian words:-

    astante
    astoria
    baratto
    cobalto
    pistola
    statale

They are to be arranged crossword puzzle fashion in the following grid:-

<img src="http://www.learnprolognow.org/html/crosswd2.eps.png" />

The following knowledge base represents a lexicon containing these words:-

    word(astante, a,s,t,a,n,t,e).
    word(astoria, a,s,t,o,r,i,a).
    word(baratto, b,a,r,a,t,t,o).
    word(cobalto, c,o,b,a,l,t,o).
    word(pistola, p,i,s,t,o,l,a).
    word(statale, s,t,a,t,a,l,e).

Write a predicate __crossword/6__ that tells us how to fill in the grid. The first three
arguments should be the vertical words from left to right, and the last three arguments the
horizontal words from top to bottom.

    crossword(V1,V2,V3,H1,H2,H3):-
        word(V1, _, V1H1, _, V1H2, _, V1H3, _),
        word(V2, _, V2H1, _, V2H2, _, V2H3, _),
        word(V3, _, V3H1, _, V3H2, _, V3H3, _),
        word(H1, _, V1H1, _, V2H1, _, V3H1, _),
        word(H2, _, V1H2, _, V2H2, _, V3H2, _),
        word(H3, _, V1H3, _, V2H3, _, V3H3, _).

However this creates duplicates, e.g.:-

    ?- crossword(V1,V2,V3,H1,H2,H3).
    V1 = astante,
    V2 = baratto,
    V3 = statale,
    H1 = astante,
    H2 = baratto,
    H3 = statale ;
    V1 = astante,
    V2 = cobalto,
    V3 = pistola,
    H1 = astoria,
    H2 = baratto,
    H3 = statale .
