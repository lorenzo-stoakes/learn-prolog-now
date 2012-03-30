<link href="http://kevinburke.bitbucket.org/markdowncss/markdown.css" rel="stylesheet"></link>

Learn Prolog Now!
=================

Notes for [chapter 4](http://www.learnprolognow.org/lpnpage.php?pagetype=html&pageid=lpn-htmlch4).

I use [swipl](http://www.swi-prolog.org/) as my prolog interpreter.

Chapter 4 - Lists
-----------------

### 4.1 Lists ###

* Just a list of items, more specifically, a finite sequence of elements.

* Some examples:-

    [mia, vincent, jules, yolanda]

We specify a list by enclosing the elements in square braces. The length of a list is the
number of elements it contains.

    [mia, robber(honey_bunny), X, 2, mia]

All sorts of prolog objects can be elements of a list. Items can occur more than once in a list.

    []

This is a special list - the empty list. Its 0 length.

    [mia, [vincent, jules], [butch, girlfriend(butch)]]

Lists can contain other lists as elements. It is length 3.

    [[], dead(z), [2, [b, c]], [], Z, [2, [b, c]]]

A combination of all these ideas.

* Any non-empty list can be thought of as consisting of two parts - the head and tail. The head
  is the first item in the list, the tail is everything else. More precisely - the tail is the
  list that remains when we take the first element away. The tail of a list is always a
  list. E.g.:-

    [mia, vincent, jules, yolanda]

The head of this list is mia, the tail is [vincent, jules, yolanda].

* What about a list of length 1? The head is the element, the tail is the empty list.

* Prolog has a built in operator, |, which can be used to decompose a list into its head and
  tail. E.g.:-

    ?- [Head|Tail] = [mia, vincent, jules, yolanda].
    Head = mia,
    Tail = [vincent, jules, yolanda].

* Only non-empty lists have heads and tails, e.g.:-

    ?- [X|Y] = [].
    false.

* We can obtain more than one element thus:-

    ?- [X, Y|W] = [[], dead(z), [2, [b, c]], [], Z].
    X = [],
    Y = dead(z),
    W = [[2, [b, c]], [], z].

* We can ignore variables via the *anonymous variable*, _.

E.g., consider that we want to get hold of the 2nd and 4th elements of the list:-

    [[], dead(z), [2, [b, c]], [], Z].

We can do it thus:-

    ?- [X1,X2,X3,X4| Tail] = [[], dead(z), [2, [b, c]], [], Z].
    X1 = [],
    X2 = dead(z),
    X3 = [2, [b, c]],
    X4 = [],
    Tail = [Z].

We've ended up a lot of information we don't care about. We can ignore this information using
the anonymous variable thus:-

    ?- [_,X,_,Y|_] = [[], dead(z), [2, [b, c]], [], Z].
    X = dead(z),
    Y = [].

* Note that _ binds independently.

* We can also use the | notation in a nested fashion:-

    ?- [_,_,[_|X]|_] = [[], dead(z), [2, [b, c]], [], Z, [2, [b, c]]].
    X = [[b, c]].
