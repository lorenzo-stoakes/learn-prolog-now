<link href="http://kevinburke.bitbucket.org/markdowncss/markdown.css" rel="stylesheet"></link>

Learn Prolog Now!
=================

Notes for [chapter 9](http://www.learnprolognow.org/lpnpage.php?pagetype=html&pageid=lpn-htmlch9).

I use [swipl](http://www.swi-prolog.org/) as my prolog interpreter.

Chapter 9 - A Closer Look at Terms
----------------------------------

9.1 Comparing Terms
-------------------

* Prolog has an important built-in predicate for comparing terms, namely __==/2__.

E.g.:-

    ?- a == a.
    true.

* Now let's compare __==/2__ with __=/2__. __=/2__ determines whether two terms can be unified.

E.g.:-

    ?- X=Y.
    X = Y.

* Let's look at queries involving instantiated variables.

E.g.:-

    ?- a=X, a==X.
    X = a.

Here, the first conjunct binds __X__ to __a__, so that when __a==X__ is evaluated, the left and
right hand sides are exactly the same Prolog object and thus __a==X__ succeeds.

* Similarly.

E.g.:-

    ?- X=Y, X==Y.
    X = Y.

Here, the first conjunct unifies __X__ and __Y__, and by the time the second is evaluated, the
two variables are exactly the same Prolog object.

* Clearly, __=/2__ and __==/2__ are different. Nonetheless, there is an important similarity -
  __==/2__ can be viewed as a stronger test than __=/2__, i.e. if __term1 == term2__ succeeds,
  then __term1 = term2__ will succeed also.

* Another important predicate to be aware of is __\\==/2__, which is defined as such that it
  succeeds precisely where __==/2__ fails.

E.g.:-

    ?- a \== a.
    false.

    ?- a \== b.
    true.

    ?- X \== Y.

9.2 Terms With a Special Notation
---------------------------------

* Sometimes Prolog treats two separate strings as identical, despite them differing,
  e.g. __foo__ and __'foo'__. This is done to make programming in Prolog more pleasant.

### Arithmetic Terms ###

* Arithmetic terms are a particularly good example of this.

E.g.:-

    ?- 2+3 == +(2,3).
    true.

    ?- +(2,3) == 2+3.
    true.

    ?- *(2,3) == 2*3.
    true.

    ?- 2*(7+2) == *(2, +(7,2)).
    true.

* In short - the familiar arithmetic notation is there for convenience, however it is, in
  effect, no different from usual term notation.

* This is equally applicable to the comparison predicates <, =<, =:=, =\=, > and >=.

E.g.:-

    ?- (2 < 3) == <(2,3).

* There is potential for ambiguity with expressions like,

E.g.:-

    ?- 2 =:= 3 == =:=(2,3).

So brackets are preferred for such situations.

E.g.:-

    ?- (2 =:= 3) == =:=(2,3).

To clarify, our equality operators are as follows.

E.g.:-

    =   The unification predicate. Succeeds only if it can unify its arguments, otherwise fails.

    \=  Negation of the unification predicate.

    ==  The identity predicate. Succeeds if its arguments are identity, otherwise fails.

    \== Negation of the identity predicate.

    =:= The arithmetic equality predicate. Succeeds if its arguments evaluate to the same integer.

    =\= The arithmetic inequality predicate. Succeeds if its arguments evaluate to different integers.

### Lists as Terms ###

* Lists are an example of Prolog providing a simplified syntax for an otherwise more cumbersome
  construct. First off, we have the head/tail thing.

E.g.:-

    ?- [a,b,c,d] == [a|[b,c,d]].
    true.

    ?- [a,b,c,d] == [a,b|[c,d]].
    true.

    ?- [a,b,c,d] == [a,b|[c,d]].
    true.

    ?- [a,b,c,d] == [a,b,c|[d]].
    true.

    ?- [a,b,c,d] == [a,b,c,d|[]].
    true.

Looking at this, it's hard to know how Prolog represents this list internally.

* In fact, list are built out of two special terms, __[]__ - the empty list, and __.__ - the
  full-stop, a functor of arity 2 which is used to build non-empty lists. These terms are
  called *list constructors*.

* The empty list is the term []. It has length 0.

* A non-empty list is any term of the form __.(term, list)__, where __term__ is any Prolog
  term, and __list__ is any list. If __list__ has length n, then __.(term, list)__ has length
  n+1.

* Looking at some examples.

E.g.:-

    ?- .(a,[]) == [a].
    true.

    ?- .(f(d,e),[]) == [f(d,e)].
    true.

    ?- .(a,.(b,[])) == [a,b].
    true.

    ?- .(a,.(b,.(f(d,e),[]))) == [a,b,f(d,e)].
    true.

    ?- .(.(a,[]),[]) == [[a]].
    true.

    ?- .(.(a,.(b,[])),[]) == [[a,b]].
    true.

    ?- .(.(a,.(b,[])),.(c,[])) == [[a,b],c].
    true.

    ?- .(.(a,[]),.(b,.(c,[]))) == [[a],b,c].
    true.

    ?- .(.(a,[]),.(.(b,.(c,[])),[])) == [[a],[b,c]].
    true.

9.3 Examining Terms
-------------------

* Let's look at some predicates which will let us examine terms more closely.

* Firstly, we'll look at predicates which test whether their arguments are terms of a certain
  type. After this, we'll look at predicates which tells us something about the internal
  structure of complex terms.

### Types of Terms ###

* Recall, there are four different *kinds*.

E.g.:-

    Variables
    Atoms
    Numbers
    Complex Terms

* Here are the tests in question.

E.g.:-

    atom/1    Is the argument an atom?
    integer/1 Is the argument an integer?
    float/1   Is the argument a floating point number?
    number/1  Is the argument an integer or a floating point number?
    atomic/1  Is the argument a constant?
    var/1     Is the argument an uninstantiated variable?
    nonvar/1  Is the argument an instantiated variable, or another term that is not an uninstantiated variable?

* Let's play.

E.g.:-

    ?- atom(a).
    true.

    ?- atom(7).
    false.

    ?- atom(loves(vincent,mia)).
    false.

    ?- atom(X).
    false.

* What if we instantiate __X__?

E.g.:-

    ?- X = a, atom(X).
    X = a.

* Integers

E.g.:-

    ?- integer(15).
    true.

    ?- integer(a).
    false.

    ?- integer(foo(bar,baz)).
    false.

    ?- X=1,integer(X).
    X = 1.

    ?- integer(X).
    false.

* Floats

E.g.:-

    float(1.5).
    true.

    ?- float(a).
    false.

    ?- float(foo(bar,baz)).
    false.

    ?- float(3).
    false.

    ?- X=1.5,float(X).
    X = 1.5.

    ?- float(X).
    false.

* Var

E.g.:-

    ?- var(X).
    true.

    ?- var(mia).
    false.

    ?- var(8).
    false.

    ?- var(3.25).
    false.

    ?- var(loves(vincent,mia)).
    false.

* Nonvar

E.g.:-

    ?- nonvar(X).
    false.

    ?- nonvar(mia).
    true.

    ?- nonvar(8).
    true.

    ?- nonvar(3.25).
    true.

    ?- nonvar(loves(vincent,mia)).
    true.

* A complex term which contains uninstantiated variables is not itself an uninstantiated variable.

So:-

    ?- var(loves(_, mia)).
    false.

    ?- nonvar(loves(_, mia)).
    true.

* You have to be careful with the order of execution.

E.g.:-

    ?- X=a, var(X).
    false.

    ?- X=a, nonvar(X).
    X = a.

    ?- var(X), X=a.
    X = a.

    ?- nonvar(X), X=a.
    false.

### The Structure of Terms ###

* We can use the predicate __functor/3__ to determine the arity and functor of a complex term.

E.g.:-

    ?- functor(f(a,b),F,A).
    F = f,
    A = 2.

* If we do this for a list, we get back __./2__:-

E.g.:-

    ?- functor([a,b],F,A).
    F = '.',
    A = 2.

* What happens if we try __functor/2__ with constants?

E.g.:-

    ?- functor(123456,F,A).
    F = 123456,
    A = 0.

    ?- functor(1.23,F,A).
    F = 1.23,
    A = 0.

* So we can use functor to examine constants, which are treated like 0-arity terms.

* We can use __functor/3__ to construct terms by specifying the second and third term and
  leaving the first undetermined.

E.g.:-

    ?- functor(T,f,7).
    T = f(_G642, _G643, _G644, _G645, _G646, _G647, _G648).

* Note that we have to specify either the first or the second and third arguments, otherwise
  Prolog will fail with an error message - this is sensible, it doesn't really make sense to
  ask for *any* value which has an arbitrary functor/arity for example.

* Note we don't have a predicate for a complex term, let's create one:-

E.g.:-x

    complexterm(X):-
        nonvar(X),
        functor(X,_,A),
        A > 0.

* What about arguments? In addition to __functor/3__, Prolog provides __arg/3__ which let's you
  access the nth argument of a complex term using __arg(n,term,value)__.

E.g.:-

    ?- arg(2, loves(vincent,mia), X).
    X = mia.

* We can instantiate an argument:-

E.g.:-

    ?- arg(2, loves(vincent,X), mia).
    X = mia.

* Accessing arguments which don't exist fails:-

E.g.:-

    ?- arg(0, loves(vincent,mia), X).
    false.

    ?- arg(3, loves(vincent,mia), X).
    false.

* We also have the __=../2__ operator which takes a complex term and returns a list with the
  functor of the complex term as the head of the list and the arguments as the tail.

E.g.:-

    ?- foo(bar,baz) =.. X.
    X = [foo, bar, baz].

    ?- =..(foo(bar,baz), X).
    X = [foo, bar, baz].

    ?- foo(bar,X) =.. Y.
    Y = [foo, bar, X].

* This predicate is also known as *univ*.

* Univ is particularly useful when you need to do something to every argument in a predicate.

### Strings ###

* Strings in Prolog are represented by a list of ASCII codes. Since it'd be a pain to have to
  use this approach to write string literals, Prolog allows you to define strings with double
  quotes.

E.g.:-

    ?- X = "foo".
    X = [102, 111, 111].

* A particularly useful predicate is __atom\_codes/2__, which converts an atom into a
  string.

E.g.:-

    ? atom_codes(foo,X).
    X = [102, 111, 111].

* You can work the other way:-

E.g.:-

    ?- atom_codes(A, "foo").
    A = foo.

* __number\_codes/2__ is also available.

E.g.:-

    ?- number_codes(345,X), X == "345".
    X = [51, 52, 53].

9.4 Operators
-------------

* Prolog lets us define our own operators.

### Properties of Operators ###

* Let's consider an expression.

E.g.:-

    is(11,+(2,*(3,3))).

We can write this as:-

    11 is 2+3*3.

* Functors that can be written between their arguments are called *infix operators*.

* Operators in Prolog, like most languages, have *precedence*, which determines which operator
  'wins out' when there is ambiguity between them.

E.g.:-

    1 + 2 * 3

* We have two different possible interpretations of this,

E.g.:-

    (1 + 2) * 3

And:-

    1 + (2 * 3)

Clearly, any sane person would prefer the latter to the former, so we need a way to resolve
this ambiguity. A means of doing this is 'precedence', which defines which operator gets
precedence when an operand could be bound to more than one.

There is some scope for confusion here - in a typical language which uses infix notation
throughout, rather than infix which ultimately translates to prefix notation (in fact a complex
term), then one candidate operator having a higher precedence than other candidate operator
means that that operator is more 'sticky', so 'wins' the operand over its opponent, in our
example * wins 2 from +.

CONFUSION: However, in Prolog *lower* precedence appears to do this, so + has higher precedence
than *, and thus + 'wins' out. This seems to somewhat make sense when you consider the prefix
representation of the above:-

    +(1, *(2, 3))

Here it's clear that + is the dominant operator.

* Another issue arises when you have equal precedence. In this situation, 'associativity' takes
  over. Operators can be left, right, or non-associative, left associative operators are
  'sticky' to the left, and right associative operators are 'sticky' to the right. For example,
  +,- and *,/ are left-associative, and ^ (raise to the power) is
  right-associative. Non-associative operators are not 'sticky' at all, so any ambiguity will
  result in an error.

E.g.:-

    1 - 2 - 3

Is equivalent to:-

    (1 - 2) - 3

Not:-

    1 - (2 - 3)

And:-

    1 ^ 2 ^ 3

Is equivalent to:-

    1 ^ (2 ^ 3)

Not:-

    (1 ^ 2) ^ 3

* In Prolog, the rule is that when an operator is left-associative, the expression to the right
  of the operator must have *lower* precedence than the operator itself, whereas the expression
  to the left can have the same precedence. The precedence of an expression is determined by
  its main operator in prefix form, or 0 if it is enclosed in brackets.

E.g.:-

    1 + 2 + 3

If we were to treat this as 1 + (2 + 3), then we'd end up with:-

    1 + +(2, 3)

I.e. an expression to the right of the + operator which has equal precedence to + (i.e. it *is*
the same operator). This is illegal for a left-associative operator. So the only alternative is:-

    +(1, 2) + 3

CONFUSION: Presumably, the 3 here has lower precedence, again.

* The type of an operator (infix, prefix or postfix), its precedence, and its associativity are
  the three things that Prolog needs to know to translate user-friendly syntax into its
  internal representation.

### Defining Operators ###

* In Prolog, you can define your own operators.

E.g.:-

    zed is_dead

Rather than:-

    is_dead(zed)

You define operators in Prolog via the following syntax:-

    :- op(Precedence, Type, Name).

* Precedence is a number between 0 and 1200, the higher the number, the greater the precedence.

* Type is an atom which specifies the type and associativity of the operator. E.g., for the +
  operator the atom is 'yfx', which states that it is infix. The 'f' here refers to the
  operator, the 'x' to an argument which has a precedence which is lower, and the y to an
  argument which has a precedence equal to or lower than the precedence of the operator.

These are the possible values for Type:-

    Infix:  xfx, xfy, yfx
    Prefix:  fx, fy
    Suffix:  xf, yf

* Some examples of inbuilt operators.

E.g.:-

    :-  op(1200, xfx,  [   :-, -->                                       ]).
    :-  op(1200,  fx,  [   :-, ?-                                        ]).
    :-  op(1100, xfy,  [    ;                                            ]).
    :-  op(1000, xfy,  [ ' ,'                                            ]).
    :-  op( 700, xfx,  [    =,  is, =.., ==, \==, =:=, =\=, <, >, =<, >= ]).
    :-  op( 500, yfx,  [    +,   -                                       ]).
    :-  op( 500,  fx,  [    +,   -                                       ]).
    :-  op( 300, xfx,  [  mod                                            ]).
    :-  op( 200, xfy,  [    ^                                            ]).

* Note that this only defines the *syntax* of the declared operator, not the *semantics*,
  i.e. - you still need to determine what the operator does once you've declared its fancy
  syntax.

E.g.:-

    :- op(500, xf, is_dead).

    kill(marcellus,zed).
    is_dead(X) :- kill(_,X).
