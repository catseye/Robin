A Short Robin Tutorial
======================

This document will lead you through writing a few simple Robin programs.
It's assumed you've written a program in some other language before.
If that other language was Lisp or Scheme or Racket, that's even better.

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

In this small Robin program, `display` is what's called a _top-level form_.
It's called this because it can only appear at the outermost level;
it can't be nested inside something.  You can have multiple top-level
forms in a file, and Robin will try to do something with all of them.
You can edit your file to say

    (display (subtract 50 42))
    (display (subtract 8 1))

And if you run it again you will get the output

    8
    7

...

(Remainder TBW.)
