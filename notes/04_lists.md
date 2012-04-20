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

```prolog
    [mia, vincent, jules, yolanda]
```

We specify a list by enclosing the elements in square braces. The length of a list is the
number of elements it contains.

```prolog
    [mia, robber(honey_bunny), X, 2, mia]
```

All sorts of prolog objects can be elements of a list. Items can occur more than once in a list.

```prolog
    []
```

This is a special list - the empty list. Its 0 length.

```prolog
    [mia, [vincent, jules], [butch, girlfriend(butch)]]
```

Lists can contain other lists as elements. It is length 3.

```prolog
    [[], dead(z), [2, [b, c]], [], Z, [2, [b, c]]]
```

A combination of all these ideas.

* Any non-empty list can be thought of as consisting of two parts - the head and tail. The head
  is the first item in the list, the tail is everything else. More precisely - the tail is the
  list that remains when we take the first element away. The tail of a list is always a
  list.

```prolog
    [mia, vincent, jules, yolanda]
```

The head of this list is mia, the tail is [vincent, jules, yolanda].

* What about a list of length 1? The head is the element, the tail is the empty list.

* Prolog has a built in operator, |, which can be used to decompose a list into its head and
  tail.

```prolog
    ?- [Head|Tail] = [mia, vincent, jules, yolanda].
    Head = mia,
    Tail = [vincent, jules, yolanda].
```

* Only non-empty lists have heads and tails.

```prolog
    ?- [X|Y] = [].
    false.
```

* We can obtain more than one element thus:-

```prolog
    ?- [X, Y|W] = [[], dead(z), [2, [b, c]], [], Z].
    X = [],
    Y = dead(z),
    W = [[2, [b, c]], [], z].
```

* We can ignore variables via the *anonymous variable*, _. E.g., consider that we want to get
  hold of the 2nd and 4th elements of the list:-

```prolog
    [[], dead(z), [2, [b, c]], [], Z].
```

We can do it thus:-

```prolog
    ?- [X1,X2,X3,X4| Tail] = [[], dead(z), [2, [b, c]], [], Z].
    X1 = [],
    X2 = dead(z),
    X3 = [2, [b, c]],
    X4 = [],
    Tail = [Z].
```

We've ended up a lot of information we don't care about. We can ignore this information using
the anonymous variable thus:-

```prolog
    ?- [_,X,_,Y|_] = [[], dead(z), [2, [b, c]], [], Z].
    X = dead(z),
    Y = [].
```

* Note that _ binds independently.

* We can also use the | notation in a nested fashion:-

```prolog
    ?- [_,_,[_|X]|_] = [[], dead(z), [2, [b, c]], [], Z, [2, [b, c]]].
    X = [[b, c]].
```

### 4.2 Member ###

* One of the most basic things we might want to know is whether something is a member of a list
  or not. This is typically called __member__. An implementation can be as follows:-

```prolog
    member(X,[X|T]).
    member(X,[_|T]):- member(X, T).
```

* Consider the following query:-

```prolog
    ?- member(zed,[yolanda,trudy,vincent,jules]).
```

* Procedurally, after much recursion and gnashing of teeth, you end up with the following
  query:-

```prolog
    ?- member(zed, []).
```

* Since, as previously discussed, the empty list cannot be split into head and tail, Prolog
  will simply return false at this point.

* We can use __member__ to determine all the elements of a list, thus:-

```prolog
    ?- member(X,[1,2,3,4]).
    X = 1 ;
    X = 2 ;
    X = 3 ;
    X = 4 ;
    false.
```

### 4.3 Recursing Down Lists ###

* The __member/2__ predicate works by recursively working its way down a list, doing something
  to the head, then recursively doing the same thing to the tail. Recursing down a list in this
  way is a very common technique in Prolog. It's worth practicing.

* We often want to compare lists to one another.

* Let's say we want to write __a2b2/2__ which determines whether 2 lists have the same length,
  e.g., we want:-

```prolog
    ?- a2b([a,a,a,a],[b,b,b,b]).
    true.

    a2b([], []).
    a2b([_|T1],[_|T2]):-
        a2b(T1,T2).
```
