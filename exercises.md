1.3 Exercises
-------------

### 1.1 ###

Which of the following sequences of characters are atoms, variables, and which are neither?

1. vINCENT

Atom

2. Footmassage

Variable

3. variable23

Atom

4. Variable2000

Variable

5. big\_kahuna\_burger

Atom

6. 'big\_kahuna\_burger'

Atom

7. big kahuna burger

Neither

8. 'Jules'

Atom

9. _Jules

Atom

10. '_Jules'

Atom

### 1.2 ###

Which of the following sequences of characters are atoms, which are vars, and which are not
terms at all? Give the functor + arity of each complex term.

1. loves(Vincent, mia)

Complex term, functor __love__, containing variable __Vincent__ and atom __mia__.

loves/2.

2. ’loves(Vincent,mia)’

Atom.

3. Butch(boxer)

Invalid, the functor must be an atom.

4. boxer(Butch)

Complex term, containing functor __boxer__, and variable __Butch__.

boxer/1.

5. and(big(burger),kahuna(burger))

__and__, __big__, __burger__, and __kahuna__ are atoms.

__big(burger)__, __kahuna(burger)__, __and(big(burger),kahuna(burger))__ are complex terms.

big/1
kahuna/1
and/2

6. and(big(X),kahuna(X))

__and__, __big__ and __kahuna__ are atoms.
__X__ is a variable.
__big(X)__, __kahuna(X)__ and __and(big(X),kahuna(X))__ are complex terms.

big/1
kahuna/1
and/2

7. _and(big(X),kahuna(X))

__\_and__, __big__ and __kahuna__ are atoms.
__X__ is a variable.
__big(X)__, __kahuna(X)__ and __\_and(big(X),kahuna(X))__ are complex terms.

big/1
kahuna/1
_and/2

8. (Butch  kills  Vincent)

This is not a valid complex term, as there is no functor, additionally this is neither an atom
nor a variable as the first character is not an alpha char.

9. kills(Butch  Vincent)

This is not a valid complex term, as there is unquoted whitespace between the two arguments.

10. kills(Butch,Vincent

This is not a valid complex term, as the parens are not closed.

### 1.3 ###

How many facts, rules, clauses and predicates are there in the following knowledge base? What
are the heads of the rules, and what are the goals they contain?

    woman(vincent).
    woman(mia).
    man(jules).
    person(X):- man(X); woman(X).
    loves(X,Y):- father(X,Y).
    father(Y,Z):- man(Y), son(Z,Y).
    father(Y,Z):- man(Y), daughter(Z,Y).

There are 3 facts:-

    woman(vincent).
    woman(mia).
    man(jules).

4 rules:-

    person(X):- man(X); woman(X).
    loves(X,Y):- father(X,Y).
    father(Y,Z):- man(Y), son(Z,Y).
    father(Y,Z):- man(Y), daughter(Z,Y).

7 clauses:-

    woman(vincent).
    woman(mia).
    man(jules).
    person(X):- man(X); woman(X).
    loves(X,Y):- father(X,Y).
    father(Y,Z):- man(Y), son(Z,Y).
    father(Y,Z):- man(Y), daughter(Z,Y).

7 predicates:-

    woman
    man
    person
    loves
    father
    son
    daughter

The heads and goals of the rules are:-

Head:-

    person(X)

Goals:-

    man(X)
    woman(X)

Head:-

    loves(X,Y)

Goals:-

    father(X,Y)

Head:-

    father(Y,Z)

Goals:-

    man(Y)
    son(Z,Y)
    daughter(Z,Y)

### 1.4 ###

Represent the following in Prolog:

Butch is a killer.

    killer(butch).

Mia and Marcellus are married.

    married(mia, marcellus).

Zed is dead.

    dead(zed).

Marcellus kills everyone who gives Mia a footmassage.

    kills(marsellus, X):- footmassage(X, mia).

Mia loves everyone who is a good dancer.

    loves(mia, X):- dancer(X), good(dancing, X).

Jules eats anything that is nutritious or tasty.

    eats(jules, X):- nutritious(X); tasty(X).

### 1.5 ###

Assuming the following knowledge base:-

    wizard(ron).
    hasWand(harry).
    quidditchPlayer(harry).
    wizard(X):- hasBroom(X), hasWand(X).
    hasBroom(X):- quidditchPlayer(X).

How does Prolog respond to the following queries:-

1. wizard(ron).

    true.

2. witch(ron).

    error - unrecognised procedure 'witch'.

3. wizard(hermione).

    false.

4. witch(hermione).

    error - unrecognised procedure 'witch'.

5. wizard(harry).

    true.

6. wizard(Y).

    harry, ron.

7. witch(Y).

    error - unrecognised procedure 'witch'.
