A Robin Tutorial
================

This tutorial will lead you through writing a few simple Robin programs.

This document is aimed at programmers who have written some programs in a
Lisp-like language such as Common Lisp or Scheme or Racket.  If you haven't,
it will probably help a lot if you work through one of the many excellent
tutorials available for these languages first.  This tutorial will
concentrate on the more unusual features of Robin, the ones that are not
shared with many other languages.

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

As of this version of Robin, there are five kinds of top-level forms:

*   `display` evaluates an expression and outputs the result.
*   `define` evaluates an expression and assigns a name to it.
*   `assert` evaluates an expression and causes the interprter
    to abort with a message, if the expression does not evaluate
    to `#t` (true).
*   `require` behaves like `assert`, but intends to signal that
    a particular symbol needs to have been given a meaning.
*   `reactor` evaluates some expressions, creates a reactor
    based on them, and installs it.

Intrinsics vs. the Standard Library
-----------------------------------

Expressions in Robin are based on a very simple definition, in which there
are only 15 intrinsic operators.  [`subtract`][] is one such intrinsic
operator.  [`add`][], by contrast, is not an intrinsic operator.  If you create
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

Functions
---------

Let's write a factorial function, and compute 5!.

    (define fact (fun (self n)
      (multiply n
        (if (gt? n 1)
          (self self (subtract n 1))
          1))))
    (display (fact fact 5))

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

If you run the above factorial program with the reference interpreter,

    bin/robin fact.robin

You'll see a message such as the following:

    (abort (inapplicable-object (abort (unbound-identifier fun))))

You might conclude from this that [`fun`][] is not built-in to the
language — and you'd be right!  Unlike almost every other Lisp-like
language, in Robin, `fun` is defined in the standard library, which
must be imported before it can be used.

It's defined as a so-called "fexpr" (which is like a macro but
even more general — more on them in a minute.)

As you saw above, you can ask the Robin reference interpreter to
load in the standard library before it runs your program:

    bin/robin pkg/stdlib.robin fact.robin

and you'll see the expected

    120

but you may notice that it's not exactly quick to come back with
that answer.

Even though they are defined in Robin, parts of the standard library
are often implemented in some other language.  This is because, while
their definition in Robin is intended to be correct and normative, it
is not necessarily efficient.  If some other, more efficient
implementation of the operation has the same semantics as the Robin
definition of the operation, they can be used interchangeably.

In particular, the reference implementation can expose, if requested,
a set of "builtins" which map to the "small" subset of the standard
library.  You can turn them on with:

    bin/robin --enable-builtins fact.robin

When you run this, you'll see it displays the answer much more promptly
this time.

Note that the reference implementation doesn't implement all of the
standard library as builtins; and note that there is no conflict if you
have it process the built-in definitions externally as well.  So this
will work just as well:

    bin/robin --enable-builtins pkg/stdlib.robin fact.robin

Fexprs
------

As we mentioned above, functions aren't intrinsic to Robin — the
`fun` operator that creates a function is defined as a so-called "fexpr"
in the standard library.

You're quite free to simply import the standard library and use `fun`
without knowing or caring that it's defined as a fexpr, whatever that
is.

However, you can write your own fexprs as well, using [`fexpr`][].

The main difference between a fexpr and a function is that a fexpr
does *not* evaluate the arguments that are passed to it.  It receives
them as an unevaluated S-expression.  What it does with this unevaluated
S-expression is completely up to it.

One trivial thing it can do with it is simply return it unmodified.
This is what the [`literal`][] operator in the standard library does,
and this is how it's defined:

    (define literal (fexpr (args env)
      args))

With this definition in place you can run

    (display (literal (hello world)))

and you'll see

    (hello world)

So `literal` is essentially the same as `quote` in Lisp or Scheme.
Except, of course, it's not intrinsic to the language.  We wrote a
fexpr to do it instead.

A fexpr defined this way also has access to the environment in which
it was called (the `env` parameter).  There is also an intrinsic
operator called [`eval`][] which evaluates a given S-expression in a
given environment.  With these tools, we can write fexprs that *do*
evaluate their arguments, just like functions.

    (define id (fexprs (args env) (eval env (head args))))
    (display (id (subtract 80 4)))

If you run this you should see 76.

When we don't care to distinguish between "functions" and "fexprs"
and "macros" we just call them all "operators".

### Recursive fexprs

(To be written).

Macros
------

Just like a function, in Robin, is a kind of fexpr, so too is a
macro a kind of fexpr.  A macro is a fexpr whose return value is
expected to be a piece of syntax which will be further evaluated
as a Robin expression.

(To be continued).

Referential transparency
------------------------

The code you've been writing inside those `define`s and `display`s
is an _expression_.  Expressions are distinct from top-level forms;
you can't say `display` inside an expression, and you can't say
`subtract` at the top level.  (This is different from a lot of
Lisps and Schemes, and it is quite intentional.)

In fact, in Robin, expressions cannot have any side-effects.  This
is sometimes called being "purely functional".

An implication of this is that all data in Robin is immutable: once
created, a list or other data structure cannot be changed.  Rather, a
new data structure, similar to the original data structure but altered
in some way, must be created.

We can be even more specific and say that in Robin, all expressions
are _referentially transparent_.  There are a number of equivalent
ways to describe what this means.  One that I find intuitive is:

> Evaluation of an operator is affected by nothing except its input
> values, and has an effect on nothing except its output value.

(I also find it reminiscent of the saying "Take only photographs,
leave only footprints", but as to whether that has any mnemonic value,
well, YMMV.)

This is a surprisingly strong guarantee.  This is good, because it
helps immensely in reasoning about programs.  But it can be surprising.

### Abort values

For example, it rules out conventional exception handling, because
conventionally, exception handling involves setting up an exception
handler, and when an exception occurs in some operator, this handler
is invoked.  But this handler is not an input value to the operator.
So the operator is relying on something that was not passed to it.
So it is not referentially transparent.

So instead of exceptions, Robin has _abort values_.  You've seen
them already as results of running some of the example code above.

An abort value is produced whenever an operator encounters an error
and can't provide a sensible value.

You can also produce one explicitly with the [`abort`][] operator:

    (display (abort (literal (something went wrong))))

Also, most operators have the following convenient behaviour:
_if any of their inputs are an abort value, they produce an abort value_.

In addition, they usually nest the abort value they received inside the abort
value they produce.  This leads to a chain of abort values.  This chain is
similar to the traceback that is provided when an uncaught exception
occurs in a procedural language such as Python or Java.

It is often quite reasonable to simply let a program evaluate to an
`abort` value, if there was an error in it — a philosophy sometimes
known as "[let it crash][]".  However, if it does become important to
recover from such an error condition, the [`recover`][] operator can be
used to intercept an abort value and achieve some alternate computation
instead.

    (display (recover
      (abort (literal (something went wrong)))
      value (list value #t)
      error (list error #f)))

Running this (with stdlib) you should see:

    ((something went wrong) #f)

which, you will note, is not an abort value.

[let it crash]: http://stratus3d.com/blog/2020/01/20/applying-the-let-it-crash-philosophy-outside-erlang/

Reactors
--------

The problem with expressions being referentially transparent is that,
in practice, software usually _does something_ over and above calculating
the result of an expression.  Users click buttons, shapes get displayed
on the screen, files get written to filesystems.

Some languages solve this problem by having functions that interact
with the outside world take a representation of the outside world as one
of their inputs, and produce a (possibly modified) representation of the
outside world as part of their output.

Robin's approach is not this.  Robin's approach is rather more like
JavaScript: it is _event-oriented_.  Events happen, and the program
reacts to them by evaluating pure functions.  These functions evaluate
to a description of actions for the system to take; and these actions
possibly cause more events to happen, and the cycle repeats.

Specifically, a Robin program may define a _reactor_ by giving an
initial state, and an operator called the _transducer_.

The transducer will be evaluated any time an event comes in.  The event could
come in from the outside world (for example, the user clicks a button,)
or it could be generated by the Robin program itself.

When the transducer is evaluated, it is passed the event, and also the
current state.  These values are passed in raw; they need no evaluation,
so the transducer is not written as a function.  Instead, the [`bind-vals`][]
operator is useful for obtaining these arguments.

The return value of the transducer consists of two parts: a new value to
use as the new current state, and a list of "commands" to issue.  Each
"command" is a two-element list, which is not really different from a new
event that will happen.

This setup is very similar to "The Elm Architecture" used in the language Elm.

Here is an example reactor.

    (reactor (line-terminal) 0
      (fexpr (args env)
        (bind-vals ((event-type event-payload) state)
          (if (equal? event-type (literal init))
            (list state
              (list (literal writeln) (literal ''Hello, world!''))
              (list (literal stop) 0))
            (list state)))))

The `(line-terminal)` part at the top is the configuration referred to above;
in this case it indicates that the reactor wishes to subscribe to a facility
called `line-terminal` (similar to "standard I/O" in most other languages.)

The `0` at the top is the initial state.  In fact this example is so simple
that the state does not change and does not make any difference to the
program, but an initial state still must be given.

After that is the transducer, given as a `fexpr`.  It receives, as its
arguments, an event, which consists of an event type and an event payload,
and the current state.  It extracts these.

It compares the event type against `init` to see if it is the initialization
event.

If it is, it returns the current state unchanged and issues two commands: a
command to write `Hello, world!` as a line of output, and a command to `stop`
the program.

If it's not, it just returns the current state unchanged.  The reactor will
wait for the next event and process it.

(To be continued).

[`abort]: ../stdlib/abort.robin
[`recover`]: ../stdlib/recover.robin
[`equal?`]: ../stdlib/equal-p.robin
[`eval`]: ../stdlib/eval.robin
[`head`]: ../stdlib/head.robin
[`if`]: ../stdlib/if.robin
[`list?`]: ../stdlib/list-p.robin
[`fexpr`]: ../stdlib/fexpr.robin
[`number?`]: ../stdlib/number-p.robin
[`operator?`]: ../stdlib/operator-p.robin
[`prepend`]: ../stdlib/prepend.robin
[`sign`]: ../stdlib/sign.robin
[`subtract`]: ../stdlib/subtract.robin
[`symbol?`]: ../stdlib/symbol-p.robin
[`tail`]: ../stdlib/tail.robin
[`add`]: ../stdlib/add.robin
[`fun`]: ../stdlib/fun.robin
[`literal`]: ../stdlib/literal.robin
[`abort`]: ../stdlib/abort.robin
