Detecting Errors in Robin
=========================

This is a somewhat philosophical write-up that overlaps with the
[Design Goals and Rationale](Rationale.md) document to some degree.

A Sea of Parentheses
--------------------

The thing about lispoids is that all those parentheses _do_ make things hard to read and hard to edit.
Take these two Scheme expressions, for instance:

    (list (+ 1 2) 3)

versus

    (list (+ 1 2 3))

They're both syntactically correct, but they have rather different meanings, and the
difference is subtle.  Deep in a sea of parentheses, the shifted parentheses might be
hard to spot.  Your only clue might be a error message from somewhere higher up in
the code, that an attempt to apply `car` failed.  Hunting it down to this shifted
parenthesis can be taxing.

This problem is a particularly bad problem in Robin, for the following reasons.

Robin has, as an ideal, that there should be as little done statically as possible.  The static
phase is: parse an S-expression, get it into a data structure.  That's all.  The rest is up
to evaluating that data structure.

There aren't even any "special forms".  The data structure you get might not be well-formed.
You might get `(bind a)`.  You can have "syntax errors" at runtime.

Even worse, Robin has, as an ideal, not to do much extra work at runtime either.  Meaning
that, some syntax oddities aren't checked for, they're just ignored.

In particular, maybe you call a fexpr with more arguments than it can understand.  Say,

    (bind a 1 (foo a) z)

In this case the `z` is ignored.

This is compounded by, maybe you said

    (bar (bind a 1 (foo a) z))

but maybe you meant

    (bar (bind a 1 (foo a)) z)

and maybe `bar` doesn't complain when you only give it one argument either.  Maybe its
second argument is optional and giving it just changes the behaviour slightly.

Deep in a sea of parentheses these differences can be hard to find.

Addressing the problem
----------------------

How do we approach this problem?

First, we recognize it.  That's why I'm writing this.

Second, we accept it as the price we have to pay for having a set of rules that are
this simple.

Third, we consider whether we want to pay that price, or if there is some other way
to achieve those ends, and if we want to pay that price instead.

To that end we might want to examine why Robin has the ideals that it does.

It's been said that Lisp isn't a programming language, it's a building material.
Under that view, Robin isn't trying to be a better programming language; it's
trying to be a more building-material-like building material.

As little as possible is done statically because we want to give the programmer the
choice of what static checking is or is not done.  By default, as little as possible
is done, and the programmer can add to that, by writing their own static analyzers,
if they like.

And we want to not do much extra work at runtime for essentially the same reason:
we want to give the programmer the choice of what checking is or is not done.
By default, as little as possible is done, and the programmer can add to it by,
for example, defining and using argument-checking wrappers, if they like.

Each of these goals by itself is fine.  It's that putting them together doesn't
work very well.

In order to live with them together we need to start giving the Robin programmer
tools to do this checking that we're leaving up to them.

But that is *also* a particularly difficult problem, because Robin is based on
fexprs.

Static analysis of fexprs
-------------------------

It has been noted many times that fexprs are hard to statically
analyze.  "Hard" isn't even the right word: they have a trivial
equational theory.  In less formal terms, if I'm a static
analyzer and you give me a fexpr, then I'm like, what do I do
with this?  I can't even tell which arguments are going to get
evaluated and which ones aren't, so how do I analyze what's in
the arguments?

Robin's approach to addressing this, which may or may not be a good one,
is to annotate operator values with some metadata which describes
what happens to the arguments, so that there is some chance of analyzing it.

Robin uses fexprs in a _definitional_, rather than _implementational_,
way.  It is expected that most fexprs will be defined in the standard
library.  So at the very least we can advertise behaviour like,
"this operator is the same as `bind` in the standard library, so
if you're an analyzer, you should treat it like that".

But not just specific operators in the standard library:
any operator defined with `fun` will come with some metadata
that says "This operator takes _n_ arguments and evaluates all
of them".

In a sense, we are supplanting the trivial equational theory of
fexprs, with our own non-trivial home-cooked equational theory that a
static analyzer can make use of.

A static analyzer is passed an environment, containing that value
and its metadata; when it sees the name that maps to that value
in the environment, it looks at the metadata and knows a bit more
how to analyze the arguments it sees.

This metadata would also be a good place for such an analyzer to
store information it deduces, such as types.

How exactly that information should be represented in the metadata
is still an open question.  In some sense it is very similar to early
Lisps associating a property list with each symbol.  But I think
ultimately it could be far more general, e.g. the metadata for an
operator could faithfully capture the complete axiomatic or
algebraic semantics of that operator, which could be used in a
proof.  But that's a long way off.

The paper "Special Forms in Lisp" advocates removing fexprs from
the Lisp language and using macros instead.

In Robin, a macro can be defined as a kind of fexpr whose return value
does not get evaluated.  This is easier for a static analyzer to
analyzer: it just expands the macro and analyzes that.

Because it is defined as a kind of fexpr in Robin, and that fexpr will
evaluate the value the macro returns (because to execute it, something
needs to), to support static analysis, the macro will need to produce
an annotation that only expands the macro, without evaluating the
result of the expansion.  Such "expand-macro" annotation could also
be used by compilers or other tools which do not need to immediately
evaluate the macro, but rather want to know how it transforms the code.

Static analysis as a library
----------------------------

Because Robin lets you write fexprs, it lets you write fexprs which
take a piece of code, check that piece of code, and if the code passes
the check, evaluates that piece of code, or otherwise evaluates to
an abort value.

In other words, you can write static analyzers as fexprs.

Such static analyzer fexprs should be available in the standard
library, i.e. there should be standard static analyses available.

The first such analyzer should simply check the arity of all the
applied operators in an expression.

That would begin to address the problem of shifted parentheses that was
brought up at the beginning of this document.
