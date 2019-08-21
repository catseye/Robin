Robin
=====

This document defines the fundamental semantics of Robin (except for the
meanings of the intrinsics: see [Intrinsics.md](Intrinsics.md) for those.)

    -> Tests for functionality "Interpret core Robin Program"

Top-level S-expressions
-----------------------

A Robin program consists of a series of "top-level" S-expressions.
Each top-level S-expression must have a particular form, but most of these
top-level S-expressions may contain general, evaluatable S-expressions
themselves.  Allowable top-level forms are given in the subsections below.

### `display` ###

`(display EXPR)` evaluates the EXPR and displays the result in a canonical
S-expression rendering, followed by a newline.

    | (display #t)
    = #t

Note that a Robin program may be split over several files in the filesystem.
Also, more than one top-level S-expression may appear in a single file.

    | (display #t)
    | (display #f)
    = #t
    = #f

### `define` ###

`(define ATOM EXPR)` defines a global name.

    | (define true #t)
    | (display true)
    = #t

You may not try to define anything that's not an atom.

    | (define #f #t)
    | (display #f)
    ? illegal top-level form: (define #f #t)

You may define multiple names.

    | (define true #t)
    | (define false #f)
    | (display false)
    | (display true)
    = #f
    = #t

Names may not be redefined once defined.

    | (define true #t)
    | (define true #f)
    ? symbol already defined: true

### `reactor` ###

`(reactor LIST-OF-ATOMS STATE-EXPR BODY-EXPR)` installs a reactor.  Reactors
permit the construction of interactive Robin programs.  See the document
[Reactor.md](Reactor.md) for more information on, examples of, and tests for reactors.

Intrinsic Data Types
--------------------

### S-expressions ###

An S-expression is a sort of catch-all data type which includes
all the other data types.  It is inductively defined as follows:

*   A symbol is an S-expression.
*   A boolean is an S-expression.
*   An integer is an S-expression.
*   A macro is an S-expression.
*   An intrinsic is an S-expression.
*   An empty list is an S-expression.
*   A list cell containing an S-expression, prepended to another list,
    is an S-expression.
*   Nothing else is an S-expression.

S-expressions have a textual representation, but not all types have values
that can be directly expressed in this textual representation.  All
S-expressions have some meaning when interpeted as Robin programs, as
defined by Robin's evaluation rules, but that meaning might be to
raise an exception to indicate an error.

### Symbol ###

A symbol is an atomic value represented by a string of  characters
which may not include whitespace or parentheses or a few other
characters (TODO: decide which ones) and which may not begin with
a `#` (pound sign) or a few other characters (TODO: decide which
ones.)

When in a Robin program proper, a symbol can be bound to a value, and
in this context is it referred to as an _identifier_.  However, if an
attempt is made to evaluate a symbol which is not an identifier,
an exception will be raised.  For a symbol to appear unevaluated in a Robin
program, it must be an argument to a macro.  For that reason, we can't
show an example of a literal symbol without first defining a macro... but
will go ahead and show the example, and will explain macros later.

    | (define literal (macro (self args env) (head args)))
    | (display (literal hello))
    = hello

A Robin implementation is not expected to be able to generate new symbols
at runtime.

Symbols can be applied, and that is a typical use of them.  But actually,
it is what the symbol is bound to in the environment that is applied.

### Booleans ###

There are two values of Boolean type, `#t`, representing truth, and `#f`,
representing falsehood.  By convention, an identifier which ends in `?`
is a macro or function which evaluates to a Boolean.  The `if` intrinsic
expects a Boolean expression as its first argument.

Booleans always evaluate to themselves.

    | (display #t)
    = #t

    | (display #f)
    = #f

Booleans cannot be applied.

    | (display (#t 1 2 3))
    ? uncaught exception: (inapplicable-object #t)

### Integers ###

An integer, in the context of Robin, is always a 32-bit signed integer.
If you want larger integers or rational numbers, you'll need to build a
bigint library or such.

For example, 5 is an integer:

    | (display 5)
    = 5

Whereas 6167172726261721 is not, and you get the 32-bit signed integer
equivalent:

    | (display 6167172726261721)
    = -878835751

Integers always evaluate to themselves.

Integers cannot be applied.

    | (display (900 1 2 3))
    ? uncaught exception: (inapplicable-object 900)

### Macros ###

A macro is an S-expression, in an environment, which describes how to
translate one S-expression to another.

One area where Robin diverges significantly from Lisp and Scheme is that,
whereas Lisp and Scheme support macro capabilities, in Robin, the macro
is a **fundamental type**.  Other abstractions, such as function values, are
built on top of macros.  Macros are "first-class" objects that may exist
at runtime and can evaluate to other macros.  Therefore, the word "macro"
has, perhaps, a slightly different meaning in Robin than in Lisp or Scheme.

They can also be compared to the one-argument `lambda` form from PicoLisp;
again, however, unlike PicoLisp's variety of `lambda` forms, Robin's
macros are the *only* abstraction of this kind fundamentally available, and
other such abstractions *must* be built on top of macros.

Whereas a function evaluates each of its arguments to values, and
binds each of those values to a formal parameter of the function, then
evaluates the body of the function in that new environment, a macro:

*   binds the macro value itself to the first formal parameter of the
    macro (by convention called `self`) — this is to facilitate writing
    recursive macros;
*   binds the literal tail of the list of the macro application to
    the second formal parameter of the macro (by convention called `args`);
*   binds a binding alist representing the environment in effect at the
    point the macro was evaluated to the third formal parameter (by
    convention called `env`); and
*   evaluates the body of the macro in that environment.

Macros are defined with the `macro` intrinsic.

Macros evaluate to themselves.

Macros are represented as the S-expression expansion of their
implementation.

    | (display
    |   (macro (self args env) args))
    = (macro (self args env) args)

Macros can be applied, and that is the typical use of them.

### Intrinsics ###

(This section needs rewriting.)

There also exist functions which cannot effectively be expressed directly
in Robin — these are the so-called _intrinsics_.  All symbols representing
intrinsics directly begin with the character ``.

One important intrinsic is `eval`.  Many macros will make use of `eval`,
to evaluate that literal tail they receive.  When they do this in the
environment in which they were called, they behave a lot like functions.
But they are not obligated to; they might evaluate them in a modified
environment, or not evaluate them at all and treat them as a literal
S-expression.

Intrinsics evaluate to themselves.

An intrinsic is represented thusly.

    | (display head)
    = head

One upshot of intrinsics is that all intrinsic Robin functionality
(excepting top-level forms) can be passed around as values.

    | (display
    |   (prepend if (prepend head ())))
    = (if head)

Intrinsics can be applied, and that is the typical use of them.

### Lists ###

A list is either the empty list, or a list cell containing a value of any
type, prepended to another list.

The "head" of a list cell is the value (of any type) that it contains;
the "tail" is the other list that it is prepended to.  The empty list
has neither head nor tail.

Lists have a literal representation in Robin's S-expression based
syntax.

The empty list is notated `()` and it evaluates to itself.

    | (display
    |    ())
    = ()

A list with several elements is notated as a sequence of those
elements, preceded by a `(`, followed by a `)`, and delimited
by whitespace.

Non-empty lists do not evaluate to themselves; rather, they represent a macro
application.  However, the `literal` macro may be used to obtain a
literal list.

    | (define literal (macro (s a e) (head a)))
    | (display
    |   (literal (7 8)))
    = (7 8)

Lists cannot be directly applied, but since a list itself represents an
application, that application is undertaken, and the result of it can
be applied.

Conventional Data Types
-----------------------

This section lists data types that are not intrinsic, but are rather
arrangements of intrinsic types in a way that follows a convention.

### Strings ###

Strings are just lists of integers, where each integer refers to a
particular Unicode codepoint.  Robin supports a sugared syntax for
specifying literal strings.  The characters of the string are given
between pairs of single quotes.

    | (define literal (macro (s a e) (head a)))
    | (display
    |   (literal ''Hello''))
    = (72 101 108 108 111)

A single single quote may appear in string literals of this kind.

    | (define literal (macro (s a e) (head a)))
    | (display
    |   (literal ''He'llo''))
    = (72 101 39 108 108 111)

Between the single quotes delimiting the string literal, a *sentinel*
may be given.  The sentinel between the leading single quote pair must
match the sentinel given between the trailing single quote pair.  The
sentinel may consist of any text not containing a single quote.

    | (define literal (macro (s a e) (head a)))
    | (display
    |   (literal 'X'Hello'X'))
    = (72 101 108 108 111)

    | (define literal (macro (s a e) (head a)))
    | (display
    |   (literal '...('Hello'...('))
    = (72 101 108 108 111)

    | (define literal (macro (s a e) (head a)))
    | (display
    |   (literal 'X'Hello'Y'))
    ? unexpected end of input

A sentinelized literal like this may embed a pair of single quotes.

    | (define literal (macro (s a e) (head a)))
    | (display
    |   (literal 'X'Hel''lo'X'))
    = (72 101 108 39 39 108 111)

By choosing different sentinels, string literals may contain any other
string literal.

    | (define literal (macro (s a e) (head a)))
    | (display
    |   (literal 'X'Hel'Y'bye'Y'lo'X'))
    = (72 101 108 39 89 39 98 121 101 39 89 39 108 111)

No interpolation of escape sequences is done in a Robin string literal.
(Functions to convert escape sequences commonly found in other languages
may one day be available in a standard module.)

    | (define literal (macro (s a e) (head a)))
    | (display
    |   (literal ''Hello\nworld''))
    = (72 101 108 108 111 92 110 119 111 114 108 100)

All characters which appear in the source text between the delimiters
of the string literal are literally included in the string.

    | (define literal (macro (s a e) (head a)))
    | (display
    |   (literal ''Hello
    | world''))
    = (72 101 108 108 111 10 119 111 114 108 100)

Adjacent string literals are not automatically concatenated.

    | (define literal (macro (s a e) (head a)))
    | (display
    |   (literal (''Hello'' ''world'')))
    = ((72 101 108 108 111) (119 111 114 108 100))

### Alists ###

An alist, short for "association list", is simply a list of two-element
sublists.  The idea is that each of these two-elements associates, in some
context, the value of its first element with the value of its second element.

### Binding Alists ###

When the first element of each two-element sublist in an alist is a symbol,
we call it a _binding alist_.  The idea is that it is a Robin representation
of an evaluation environment, where the symbols in the heads of the sublists
are bound to the values in the tails of the pairs.  Binding alists can be
created from the environment currently in effect (such as in the case of the
third argument of a macro) and can be used to change the evaluation
environment that is in effect (such as in the first argument to `eval`.)

TODO: binding alists may be replaced by abstract map objects of some kind.

Comments
--------

Any S-expression preceded by a `;` symbol is a comment.  It will still
be parsed, but it will be ignored.

    | (display
    |   ;(this program produces a list of two booleans)
    |   (prepend #f (prepend #f ())))
    = (#f #f)

Because S-expressions may nest, and because comments may appear
inside S-expressions, comments may nest.

    | (display
    |   ;(this program produces
    |     ;(what you might call)
    |     a list of two booleans)
    |   (prepend #f (prepend #f ())))
    = (#f #f)

Comments are still parsed.  A syntax error in a comment is an error!

    | (display
    |   ;(this program produces
    |     #k
    |     a list of booleans)
    |   (prepend #f (prepend #f ())))
    ? (line 3, column 6):
    ? unexpected "k"
    ? expecting "t" or "f"

Any number of comments may appear together.

    | (display
    |   (prepend ;what ;on ;earth #f (prepend #f ())))
    = (#f #f)

Comments may appear before a closing parenthesis.

    | (display
    |   (prepend #f (prepend #f ()) ;foo))
    = (#f #f)

    | (display
    |   (prepend #f (prepend #f ()) ;peace ;(on) ;earth))
    = (#f #f)

Comments may appear in an empty list.

    | (display
    |   ( ;hi ;there))
    = ()

Comments need not be preceded by spaces.

    | (display
    |   (;north;by;north;west))
    = ()

To put truly arbitrary text in a comment, the string sugar syntax may be
used.

    | (display
    |   ;''This program, it produces a list of two booleans. #k ?''
    |   (prepend #f (prepend #f ())))
    = (#f #f)
