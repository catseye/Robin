Robin's Approach to Static Analysis
===================================

This document is a draft.

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

`pure`
------

Probably the most sensible place to start with all this is a macro which
defines a function if and only if it can prove that the function has no
side-effects.  (Otherwise, it presumably raises an exception.)

This goes along with the convention that the names of such functions end
with `!`.  However, we'd like to ensure this at the level of values rather
than names; in Robin, values have types, but names do not.

As described in the Style document, the only "side-effects" in Robin are
spawning a process, sending a message to a process, and receiving a message
from a process, and these can be distinguished at a lower level than simply
"might this have side-effects or not".  But for now, for simplicitly, I'm
going to glom them all together under this banner.

One problem I've faced is finding a succinct adjective to describe functions
which can not possibly cause side-effects.  "Referentially transparent" is
one of the more official adjectives, but it's far too long.  I've
provisionally settled on *pure*, even though it is far from the most
descriptive word.

So we can imagine our static analyzer, with its provisional name, like so.
We can define a macro called, say, `pure-fun`.  It accepts the same kinds
of arguments as `fun`:

    (bind perimeter (pure-fun (w h) (* 2 (+ w h)))
        ...)

`pure-fun` however, examines its second argument in detail before
evaluating to a function value which implements this function.  It looks up
`*` in its environment, sees that there is metadata on the value referred
to `*` that indicates that it is pure, and continues.  It descends into the
term, and sees that `2`, being a literal value, is pure; it sees that `+`
is also pure; and it sees that `w` and `h` are arguments to the function.
(If these aren't pure, that's not a problem with this function per se.)
Having thus proven the expression to be pure, it evaluates the function
value in the exact same way that `fun` would, then adds metadata to that
value that marks it as `pure`.

Then `bind` binds the identifier `perimeter` to this value, which has
been marked as `pure`; so when we look up `perimeter` in this environment,
we know it refers to a pure function.  We can use this information in
subsequent checks, like:

    (bind perimeter (pure-fun (w h) (* 2 (+ w h)))
      (bind psquare (pure-fun (w) (perimeter w w))
        ...))

This is all well and good for functions, but for other macros, we may
need to do more work.  Specifically, a macro like `fun` itself, which
defines a custom syntax, might need to describe what their syntax is
like, in their metadata, so that the purity analyzer can recognize them
and process them correctly.

`constant`
----------

On top of this we can easily build another level of static analysis,
`constant`.  An expression is constant if it is a literal, or if it
an application of a pure, constant function to constant arguments.

`constant` and `pure` actually feed back into each other, which makes
this even more complex: the `eval`ing of a `constant` value inside a
function may make that function `pure`, whereas if it is not a `constant`
value, there might be no way to prove this.
