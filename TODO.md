TODO
====

(Note, some of these are possibly long-term plans.)

The term "macro"
----------------

What we are currently calling a "macro" is actually a "fexpr"
(for lack of a better name.)  Some Lisps (newLISP?) call it
a macro too, but this is also a misleading name, based on
what programmers expect from something called a "macro".

Just as we have defined `fun` in terms of fexprs, we can
define `macro` in terms of fexprs.  (It is expected to
return a literal chunk of program, which we then evaluate.)

Static analysis of fexprs
-------------------------

It has been noted many times that fexprs are hard to statically
analyze.  "Hard" isn't even the right word: they have a trivial
equational theory.  In less formal terms, if I'm a static
analyzer and you give me a fexpr I'm like, what can I say about
this?  This thing could be doing literally _anything_ with its
arguments...

The idea so far is to be able to decorate operator values with
some metadata which describes the equational theory that should
apply to them.

So, any operator defined with `fun` will come with some metadata
that says "This operator takes _n_ arguments and evaluates all
of them".

A static analyzer is passed an environment, containing that value
and its metadata; when it sees the name that maps to that value
in the environment, it looks at the metadata and knows a bit more
how to analyze the arguments it sees.

This metadata would also be a good place for such an analyzer to
store information it deduces, such as types.

How exactly that information should be represented in the metadata
is an open question.  In some sense it is very similar to early
Lisps associating a property list with each symbol.  But I think
ultimately it could be far more general, e.g. the metadata for an
operator could faithfully capture the complete axiomatic or
algebraic semantics of that operator, which could be used in a
proof.  But that's a long way off.

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

`(compose f1 f2)` to compose two operators.

`(modulo x y)` which is always positive, change `remainder` to
be sign of divisor (like R6RS.)

Other libs
----------

`lispish` lib, that just gives you all the old jargon-y names
as aliases.

Static analysis lib.  This is its own bucket of worms.  It should
expose a macro that can be wrapped around an arbitrary standard
Robin program _p_ and, if static analysis of _p_ is successful,
simply evaluates to whatever _p_ evaluates to.  And if not
successful, produces an abort.  Should probably start small --
statically check the arity of every application, for instance.
Note that this relies on the assumption that all standard symbols
have their standard meanings.

TopLevel
--------

`rename` form.

Testing
-------

Some way to make it easier to test reactive programs, i.e.
some way to provide mock facilities.  (Actually, do we need this?
Or rather, how reactor-specific is this?  You can just test the
transducer itself, feeding it a set of mock events.)

The QuickCheck tests for equivalency don't seem to be very strong.  They might
even be outright wrong.  Generally, lots and lots more could be done with
the QuickCheck tests.

Documentation
-------------

Finish the tutorial (recursive macros, reactors).

Reactors
--------

Perhaps namespace each event name.

A more involved environment, with more facilities than just
`line-terminal`.  Multiplexed I/O would be a start, could write an
IRC client.  Could also define an environment for web pages.

For that matter, `HasteMain.hs` should inject facilities that
only make sense in the HTML5 DOM.

Subscription and unsubscription from facilities using standard commands.

More elegant way for handling abort responses (they should not
trigger events -- because this can lead to a cascade of abort
responses, if the reactor is erroneously handling those events too --
but they should be displayed if requested.)

Allow only one reactor.  (Its transducer can be composed from
multiple operators, instead of having multiple reactors.)

Example programs
----------------

Hunt the Wumpus.

Implementation of a concatenative language in an idiomatic style,
taking a list like `(1 2 + 3 *)` and folding `compose` over it to
obtain a function, and applying that function.

Rename
------

Rename "small" to "base".

Rename `fun` to `function`.  This is because Robin prefers full words
over abbreviations, which are jargon-y.

Distribution
------------

Probably need to bite the bullet and add a `cabal` or `stack`
package descriptor.

    cabal install random-1.1
    cabal install parsec-3.1.1
    cabal install QuickCheck-2.13.2

