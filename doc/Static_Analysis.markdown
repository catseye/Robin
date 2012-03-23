Robin's Approach to Static Analysis
===================================

This document is a draft.  It contains a bunch of not-necessarily
well-integrated notes on how Robin approaches the problem of static
analysis.

Static Analysis
---------------

Robin itself does not define any system of static analysis.  It has a
(latent) system of types, but does not require any type mismatch to
be detected at any point in time before the expression involving that
type mismatch is evaluated.

There is a reason for this.  Just as functionality is not built-in
to the language but rather provided in modules, so too is static
analysis not built-in to the language; different type systems (and
other sets of correctness rules) of differing strengths can be applied
in a modular fashion to Robin programs (and, in a fine-grained fashion,
to individual Robin modules.)

(This already happens with other languages, but in a perhaps less
structured fashion.  The classic tool `lint` analyzes C programs using
a type system more restrictive than that defined by C itself [cite
Dragon book here].  The tool `pyflakes` checks more rules about Python
programs than the Python language defines, and so forth.)

At the same time, I'd like Robin itself to be amenable to various
forms of static analysis.  That Robin itself defines very few rules for
static correctness should not be taken as advocacy for dynamic typing;
rather, it should be taken as advocacy for letting each developer choose
the level of static analysis they wish to apply.

`macro` and `eval`
------------------

The choice of `macro` as one of the fundamental abstractions, and the
corresponding requirement to use `eval` to implement things like
functions on top of it, seem to pose some barriers to static analysis.

And, they do make some things harder.  But, these barriers are not
insurmountable.

One of the difficulties of `eval` in non-homoiconic languages (for
example, `exec` in Python) is that the expression being evaluated
exists as a string of Python code.  To analyze it, it must be parsed.
Since, in Robin, the thing being `eval`ed is simply a term, no parsing
need take place.

In a sense, we're `eval`ing things everywhere anyway; `eval` is simply
an explicit invokation of this on a term which is not known in advance.
We can say, rather, that it is not known *entirely* in advance.  If we
know some properties about term we are `eval`ing, we can know more
about what to expect of the result of its evaluation.  In other words,
we can use static analysis to help us (somewhat) in our static analysis.

As a more concrete example, if we have a macro which `eval`s one of its
arguments in an unaltered environment (i.e. the very same value that
the macro was passed in its third argument,) we know just as much about
that evaluation as we would any argument to any function.  A static
analyzer can detect this case by checking what it knows about the values
of the bindings we are passing to `eval`.

And of course, it is always an option to punt.  There will always be
terms that have some property, but which no static analyzer can prove
have that property.  If it is simply too complex to prove some property
of `macro` expressions, the static analyzer can always say it doesn't
know; it is still useful on programs, or parts of programs, which do
not contain `macro` expresions.

Constructing Static Analyzers
-----------------------------

We can take the "static analyses are modules" paradigm quite far in
Robin.  Since Robin allows metadata to be associated with every value,
we may record properties we have proved about a value on that value,
and use these established properties to check further proofs of properties.

In other words, we can write Robin macros which statically check their
contents, and only evaluate to a value if their contents pass that
check.  In this sense, different static analyses really *could* be
available in different Robin modules.  For example, a module `total`
could export an identifier `fun` which shadows `small`'s `fun`, and
which only evaluates to a function value if it can prove that the
function being defined always halts.  (Otherwise, it would presumably
raise an exception.)

In this way, Robin could provide tools for static analysis, of the same
sort that mainstream languages like Java and C and Haskell support, while
not abandoning the "it's not a programming language, it's a building
material" flexibility of it's homoiconic Lisp and Scheme heritage.  In
addition (as I've already mentioned, but just to stress the point,) any
number of different static analyses, of different strengths, for different
purposes, could be applied to different modules (or even perhaps different
parts of the same program), giving the developer a wide range of choices
between flexibility and confidence in correctness.

I don't expect this will be easy, of course, but I do hope it is somehow
possible.

Some Implications of this Approach
----------------------------------

The "static" in "static analysis" means that the code is analyzed without
running it; it does not strictly mean that it happens *before the code is
run*.  Since, in Robin, you (the programmer) write static analyzers in
Robin, then it's your responsibility to make sure they run when you want
them to run (and yes, typically this will be before the code itself is
run; otherwise you might as well just not have any static analysis, and
wait for incorrect code to crash when it runs.)

Fortuitously, there is a convenient time to run static analyses on a
module -- when that module is imported.  The code to construct the
environment exported by the module needs to run at that point anyway, and
the static analysis might as well run then too.  This also means that
statically analyzing a set of a modules is as simple as constructing a
test program which simply imports all of those modules; if any of them
fail static analysis, an exception will be thrown at import time:

    (robin (0 1) ((foo (0 1) *) (bar (0 1) *) (baz (0 1) *))
      #t)

There is also room here for optimization.  If static analysis for a module
happens at module-import time, the implementation, after successfully
importing a module, can create an equivalent version of the module by
replacing each instance of a checked function definition with an unchecked
function definition.  This version will be weaker from an analysis
perspective, but that won't matter, since the equivalent module has already
been checked; and the weaker module will be more efficient, at least during
import.

Another implication of running static analyzers "in" the code they are
statically analyzing -- and this relates to having the option to punt --
is that they, like the rest of your program, may not be perfect.  In the
end, they are constructed from the same goo as the rest of your program,
and to have confidence that those goo formations do what you expect them
to, you should have tests for them.  If a static analyzer is imperfect,
this just means you can't rely on it as heavily as you could if it was
perfect; it doesn't mean you can't still use it.  When you think about it,
the static analyzer in some Java compiler you're using might contain a bug,
too, but such a bug might be harder for you to fix, what with that compiler
possibly being written in a different language.  (The flip side of this is
that Robin's static analyzers ought, in some cases at least, to be able to
analyze themselves easily -- leading to better confidence that the analyzers
themselves are correct.)

Yet another implication is the impact this splitting off of static analysis
from the language per se has on coding style.  Often, the simplest way to
code a function does not handle errors well, or consistently.  Take, for
example, `export`.  It would be useful, from a software engineering stand-
point, to be made aware of when you're trying to `export` a binding which
doesn't exist (maybe you made a typo.)  But the most straightforward way
to implement `export` is as a `filter` over the environment.  Making
`export` raise an exception if one of the identifiers you listed doesn't
exist in the environment would mean adding a dynamic check to the
implementation of `export` that roughly doubles its complexity.  It seems
that the Robin approach would be to avoid that complexity and keep the
definition of `export` simple, even though it is not quite as useful as it
could be; and make up for it with a static analysis to catch such problems
later on in the game.

Concrete Applications
---------------------

Let's provide an overview of some of the possible concrete applications of
static analysis in Robin.

`consistent`
------------

An expression is consistent if each identifier used in the expression is
bound to some value in the environment in effect in the place that it is
used.

This is a fairly simple analysis (and probably should have been the one
I started working on first, instead of `pure`.)

`non-shadowing`
---------------

An expression is non-shadowing if it does not redefine any names, i.e.
it does not bind a value to an identifier to which a value is already
bound.

`pure`
------

A macro is referentially transparent if, for every set of possible particular
actual arguments, it always evaluates to the same particular result value.

"Referentially transparent" is a bit of a mouthful, so in Robin, we (for
better or worse) call macros with this property *pure*.

There is a convention that macros that are not pure are bound to names that
end with an exclamation point (`!`).  However, this is a convention, and so
is not enforced; also, purity analysis deals with the values themselves, not
the names they may or may not be bound to.

For more information on `pure`, see [the documentation for the `pure`
module](module/Pure.falderal).

`constant`
----------

On top of `pure` we can easily build another level of static analysis,
`constant`.  An expression is constant if it is a literal, or if it
an application of a pure, constant function to constant arguments.

`constant` and `pure` actually feed back into each other, which makes
this even more complex: the `eval`ing of a `constant` value inside a
function may make that function `pure`, whereas if it is not a `constant`
value, there might be no way to prove this.

`total`
-------

A macro is total if it always terminates.  In the general case, this is
undecidable.  However, it is not too difficult to analyze a macro and
determine that it is primitive recursive, and all primitive recursive
functions are total.

`non-raising`
-------------

An expression is non-raising if executing it cannot possibly raise an
exception.  (The simplest way to achieve this is to catch everything at
a high level, and not raise anything in the exception handler.)
Java does an analysis similar to this.
