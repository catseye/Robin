TODO
====

(Note, some of these are possibly long-term plans.)

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

`(bound? sym)` returns `#t` if the symbol is bound, `#f` if not.

`(compose f1 f2)` composes two functions.

`macro?` should return `#t` on intrinsics, because they are
intrinsic macros.  Maybe a separate `intrinsic?` predicate.

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

Toplevels
---------

`(require foo)` is conceptually sugar for `(assert (bound? foo))`
but doesn't need `bound?` to be defined, and is also declarative,
so an implementation can read it and perhaps try to find `foo`
and load it.

The intrinsics defining files need not be entirely empty; they
should `(requires thesym)` because, being an intrinsic, it should
be implemented, and available, and if it's not, it should fail.

Tests
-----

"Execute", not "Interpret", a Robin program (as these tests could also
be used to test compilers.)

A way to evaluate a Robin expression and display it, mainly
to make the tests more concise - don't need to say `(display ...)` always.

Reactors
--------

Perhaps namespace each event name.

A more involved environment, with more facilities than just
`line-terminal`.  Multiplexed I/O would be a start, could write an
IRC client.  Could also define an environment for web pages.

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

Rename `raise` to `throw`.  This is because `throw` is the opposite
of `catch`.  Also "raise" suggests an error, but it might be merely a
non-local exit.
