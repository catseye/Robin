A Short Robin Tutorial
======================

This document will lead you through writing a few simple Robin programs.

We will assume you've written some programs in a Lisp-like language
such as Scheme or Racket or Irken.  If you haven't, it will probably
help a lot if you work through one of the many excellent tutorials
available for these languages.

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
are only 15 intrinsic operations.  `subtract` is one such intrinsic
operation.  `add`, by contrast, is not an intrinsic.  If you create a program

    (define one 1)
    (display (add 8 one))

and run it with

    bin/robin sum.robin

you'll get an error like

    Main.hs: uncaught exception: (unbound-identifier add)

However, Robin does come with a standard library of operations, which are
defined in terms of intrinsics.  You need to tell Robin to load this standard
library before you can use any of the operations in it.  So, if you run,

    bin/robin pkg/stdlib.robin sum.robin

You'll see the answer you probably expected,

    9

Builtins
--------

Even though they are defined in Robin, parts of the Standard Library
are often implemented in some other language.  This is because their
definition in Robin is intended to be correct and normative, not
necessarily efficient.  If the implementation of the operation in some
other language has the same semantics as the Robin definition of the
operation, it can be used interchangeably, and more efficiently.

In particular, the reference implementation exposes, by default,
a set of "builtins" which map to the "small" subset of the standard
library.  You can turn them off with:

    bin/robin --no-builtins difference.robin

`difference.robin` doesn't rely on any of these builtins, so it
still runs.

...

(Remainder TBW.)
