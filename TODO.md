TODO
====

(Note, some of these are possibly long-term plans.)

Checking number of parameters
-----------------------------

This is a tricky subject.

The list of parameters is naturally a list.  Robin in some
ways tries to deny that, rather than embracing it as Lisp
and Scheme do.

It does so in the name of "correctness".  It is incorrect
to pass more arguments to a function, than it expects,
so you should be informed of this by means of an exception.

But it goes further, and thinks: no function should have
a variable number of arguments.  Instead, you should pass
it a list.

I guess the reason is, to make analysis of that easier.
Possibly also, if there was a sugared syntax, to make that
syntax easier (each function has constant number of parameters
means the parameters can be parsed deterministically, without
needing extra syntax such as parens.)

But if we give this up we get a different kind of simplicity.
Functions like `add` and `multiply` naturally take any number of
parameters.  `compose` would too.

But, we have different words for those, too: `sum` and `product`
(and for functions it could be... `pipe`?)  So there is a reason
for there to be distinct functions that take lists.

Disjointness of types
---------------------

Insofar as the types of Robin are disjoint, we can have a
`typeof` intrinsic which returns a literal symbol such as
`number`, `list`, and so forth, instead of _n_ predicates.
In practice the predicates are useful, and would be retained
in the stdlib.  But this could make the set of intrinsics smaller.

Opaque type
-----------

Values of "opaque" type.  Intrinsics `wrap` and `unwrap`.  But note
that only the functions implementing the ADT are guaranteed to be able
to unwrap an opaque value, and those functions might be implemented
natively, rather than as Robin code that uses `wrap` and `unwrap`.

Then, can we provide environments as an abstract type?  We would
also provide `env->alist` and `alist->env` so they can be worked
with easily.  But they could be used natively to support symbol
lookup, and if they are implemented as trees or hash tables, this
might have performance benefits.  (Or perhaps not.)

Opaque type might also be useful for internals, e.g. signaling
to runtime system that an error occurred.

Stdlib
------

Rename "small" to "core" or "base" or something.

`(compose f1 f2)` composes two functions.

`(sgn x)`

`(modulo x y)` which is always positive, change `remainder` to
be sign of divisor (like R6RS.)

Other libs
----------

`schemeish` lib, that just gives you all the old jargon-y names
as aliases.

Static analysis lib.  This is its own bucket of worms.  It should
expose a macro that can be wrapped around an arbitrary standard
Robin program _p_ and, if static analysis of _p_ is successful,
simply evaluates to whatever _p_ evaluates to.  And if not
successful, raises an error.  Should probably start small --
statically check the arity of every application, for instance.
Note that this relies on the assumption that all standard symbols
have their standard meanings.

Tests
-----

Grep for FIXME in stdlib.

Reactors
--------

Perhaps namespace each event name.

A more involved environment, with more facilities than just
`line-terminal`.  Multiplexed I/O would be a start, could write an
IRC client.  Could also define an environment for web pages.

For that matter, `HasteMain.hs` should inject facilities that
only make sense in the HTML5 DOM.

Subscription and unsubscription from facilities using standard commands.

Example programs
----------------

Hunt the Wumpus.

Implementation of a concatenative language in an idiomatic style,
taking a list like `(1 2 + 3 *)` and folding `compose` over it to
obtain a function, and applying that function.

Rename
------

Rename `fun` to `function`.  This is because Robin prefers full words
over abbreviations, which are jargon-y.

Rename `>` to `greater-than?`, and so forth.  Figure out a place to
put the aliases-which-are-punctuation-y.

Rename `raise` to `throw`.  This is because `throw` is the opposite
of `catch`.  Also "raise" suggests an error, but it might be merely a
non-local exit.
