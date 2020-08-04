A Short Robin Tutorial
======================

This tutorial will lead you through writing a few simple Robin programs.

This document is aimed at programmers who have written some programs in a
Lisp-like language such as Scheme or Racket or [Irken][].  If you haven't,
it will probably help a lot if you work through one of the many excellent
tutorials available for these languages first.  This tutorial will
concentrate on the more unusual features of Robin, the ones that are not
shared with many other languages.

[Irken]: https://github.com/samrushing/irken-compiler/blob/master/docs/intro.md

Basic usage
-----------

First, if you haven't done so, obtain a Robin interpreter for your
system.  The [README](../README.md) gives instructions for obtaining
and building the reference interpreter, `robin`.  We'll use it in
these examples.

A Robin program is usually saved as a plain text file on your computer.
Open up a text editor, enter the following, and save it as
`difference.robin`:

    (display (subtract 50 42))

Now if you run the Robin interpreter on this file by typing, in your
terminal:

    bin/robin difference.robin

You will see it display a number

    8

which is the answer to the question, "What is 50 minus 42?".

Top-level forms
---------------

In this small Robin program, `display` is what's called a _top-level form_.
It's called this because it can only appear at the outermost level;
it can't be nested inside something.  You can have multiple top-level
forms in a file, and Robin will try to do something with all of them.
You can edit your file to say

    (define one 1)
    (display (subtract 8 one))

And if you run it again you will get the output

    7

As of this version of Robin, there are only 4 kinds of top-level
forms:

*   `display` evaluates an expression and outputs the result.
*   `define` evaluates an expression and assigns a name to it.
*   `assert` evaluates an expression and causes the interprter
    to abort with a message, if the expression does not evaluate
    to `#t` (true).
*   `reactor` evaluates some expressions, creates a reactor
    based on them, and installs it.

Intrinsics vs. the Standard Library
-----------------------------------

Expressions in Robin are based on a very simple definition, in which there
are only 15 intrinsic operators.  `subtract` is one such intrinsic
operator.  `add`, by contrast, is not an intrinsic operator.  If you create
a program

    (define one 1)
    (display (add 8 one))

and run it with

    bin/robin sum.robin

you'll get an error like

    (abort (unbound-identifier add))

However, Robin does come with a standard library of operations, which are
defined in terms of intrinsics.  You need to tell Robin to load this standard
library before you can use any of the operations in it.  So, if you run,

    bin/robin pkg/stdlib.robin sum.robin

You'll see the answer you probably expected,

    9

(Actually this isn't the way the reference interpreter behaves right now.)

Functions
---------

Let's write a factorial function, and compute 5!.

    (define fact (fun (self n)
      (multiply n
        (if (gt? n 1)
          (self self (subtract n 1))
          1))))
    (display (fact fact 5))

`multiply` is not an intrinsic, so you'll need to run this with

    bin/robin pkg/stdlib.robin fact.robin

Some of this definition is probably what you would expect from a
recursive definition of factorial in any Lisp-like language.
However, some of it is probably not.

It comes from the fact that Robin has no way to make a forward
reference — no `letrec` or equivalent.  At the time the definition
of `fact` is being read, the `fact` symbol is not yet defined,
so `fact` cannot call itself directly.

So we give `fact` a way to call itself by following a convention:
whenever `fact` is called (including when `fact` needs to call
itself), the `fact` function itself is passed as the first argument
to `fact`.  By convention we call this parameter `self`.

This is related to the so-called Y combinator.

Builtins
--------

Even though they are defined in Robin, parts of the Standard Library
are often implemented in some other language.  This is because, while
their definition in Robin is intended to be correct and normative, it
is not necessarily efficient.  If some other, more efficient
implementation of the operation has the same semantics as the Robin
definition of the operation, they can be used interchangeably.

In particular, the reference implementation exposes, by default,
a set of "builtins" which map to the "small" subset of the standard
library.  You can turn them off with:

    bin/robin --no-builtins pkg/stdlib.robin fact.robin

If you do this, you'll see

    Main.hs: uncaught exception: (unbound-identifier fun)

You might conclude from this that `fun` is not a built-in — and you'd
be right!  Unlike basically every other Lisp-like language, in Robin,
`fun` is a derived form.  It's implemented as a macro.

(Actually this isn't the way the reference interpreter behaves right now.)

Macros
------

(To be written).

Reactors
--------

(To be written).
