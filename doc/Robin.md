Robin
=====

This document defines version 0.8 of the Robin programming language.
In this document, the name "Robin" by itself refers to the Robin
programming language version 0.8.

The Robin specification is modular in the sense that it consists
of several smaller specifications, some of which depend on others,
that can be composed or used in isolation.  These specifications are:

*   Part 0. Robin Syntax
*   Part 1. Robin Expression Language
    *   (a) Evaluation Rules
    *   (b) Intrinsic Data Types
    *   (c) Conventional Data Types
    *   (d) Standard Environments
*   Part 2. Robin Toplevel Language
*   Part 3. Robin Reactors

Robin Expressions and Robin Toplevels are written in the Robin Syntax.
A Reactor is defined with Robin Expressions.  A Toplevel contains
Expressions used for various purposes, including Reactors.

Note that, although each part of the specification builds on the
parts before it, it is not really possible to give testable examples
of some of the parts, without referring to parts that have not yet
been seen.  (For example, when describing Robin Syntax, we would
like to show that it allows one to write Robin Expressions.)  Thus
many of these examples are given in the Robin Toplevel Language,
even though they need not strictly be.

Part 0. Robin Syntax
--------------------

    -> Tests for functionality "Execute core Robin Toplevel Program"

### S-expressions ###

Robin is an S-expression based language, so it has a syntax similar to
Common Lisp, Scheme, Racket, and so forth.

The basic grammar of these S-Expressions, given in EBNF, is:

    SExpr  ::= [";"] (symbol | number | boolean | Quoted | "(" {SExpr} ")")
    Quoted ::= "'" sentinel "'" arbitrary-text-not-containing-sentinel "'" sentinel "'"

A symbol is denoted by a string which may contain only alphanumeric
characters and certain other characters.

A number is denoted by a string of decimal digits.

A boolean is denoted `#t` or `#f`.

A sentinel is any string not containing a single quote (`'`).  In a Quoted
production, the start sentinel and the end sentinel must match.

### Arbitrary literal strings

Robin supports a sugared syntax for specifying literal strings.
The characters of the string are given between pairs of single quotes.
Such a form is parsed as a conventional string data type (see
the "String" section in the Robin Expression Language for details.)

    | (define literal (macro (a e) (head a)))
    | (display
    |   (literal ''Hello''))
    = (72 101 108 108 111)

A single single quote may appear in string literals of this kind.

    | (define literal (macro (a e) (head a)))
    | (display
    |   (literal ''He'llo''))
    = (72 101 39 108 108 111)

Between the single quotes delimiting the string literal, a *sentinel*
may be given.  The sentinel between the leading single quote pair must
match the sentinel given between the trailing single quote pair.  The
sentinel may consist of any text not containing a single quote.

    | (define literal (macro (a e) (head a)))
    | (display
    |   (literal 'X'Hello'X'))
    = (72 101 108 108 111)

    | (define literal (macro (a e) (head a)))
    | (display
    |   (literal '...('Hello'...('))
    = (72 101 108 108 111)

    | (define literal (macro (a e) (head a)))
    | (display
    |   (literal 'X'Hello'Y'))
    ? unexpected end of input

A sentinelized literal like this may embed a pair of single quotes.

    | (define literal (macro (a e) (head a)))
    | (display
    |   (literal 'X'Hel''lo'X'))
    = (72 101 108 39 39 108 111)

By choosing different sentinels, string literals may contain any other
string literal.

    | (define literal (macro (a e) (head a)))
    | (display
    |   (literal 'X'Hel'Y'bye'Y'lo'X'))
    = (72 101 108 39 89 39 98 121 101 39 89 39 108 111)

No interpolation of escape sequences is done in a Robin string literal.
(Functions to convert escape sequences commonly found in other languages
may one day be available in a standard module.)

    | (define literal (macro (a e) (head a)))
    | (display
    |   (literal ''Hello\nworld''))
    = (72 101 108 108 111 92 110 119 111 114 108 100)

All characters which appear in the source text between the delimiters
of the string literal are literally included in the string.

    | (define literal (macro (a e) (head a)))
    | (display
    |   (literal ''Hello
    | world''))
    = (72 101 108 108 111 10 119 111 114 108 100)

Adjacent string literals are not automatically concatenated.

    | (define literal (macro (a e) (head a)))
    | (display
    |   (literal (''Hello'' ''world'')))
    = ((72 101 108 108 111) (119 111 114 108 100))

### Comments

    -> Tests for functionality "Evaluate core Robin Expression"

Any S-expression preceded by a `;` symbol is a comment.  It will still
be parsed, but it will be ignored.

    | (
    |   ;(this expression evaluates to a list of two booleans)
    |   prepend #f (prepend #f ()))
    = (#f #f)

Because S-expressions may nest, and because comments may appear
inside S-expressions, comments may nest.

    | (
    |   ;(this expression evaluates to
    |     ;(what you might call)
    |     a list of two booleans)
    |   prepend #f (prepend #f ()))
    = (#f #f)

Comments are still parsed.  A syntax error in a comment is an error!

    | (
    |   ;(this expression evaluates to
    |     #k
    |     a list of booleans)
    |    prepend #f (prepend #f ()))
    ? (line 3, column 6):
    ? unexpected "k"
    ? expecting "t" or "f"

Any number of comments may appear together.

    | (prepend ;what ;on ;earth #f (prepend #f ())))
    = (#f #f)

Comments may appear before a closing parenthesis.

    | (prepend #f (prepend #f ()) ;foo))
    = (#f #f)

    | (prepend #f (prepend #f ()) ;peace ;(on) ;earth))
    = (#f #f)

Comments may appear in an empty list.

    | ( ;hi ;there))
    = ()

Comments need not be preceded by spaces.

    | (;north;by;north;west))
    = ()

To put truly arbitrary text in a comment, the string sugar syntax may be
used.

    | (;''This expression, it evaluates to a list of two booleans. #k ?''
    |  prepend #f (prepend #f ()))
    = (#f #f)

Part 1. Robin Expression Language
---------------------------------

(a) Evaluation Rules
--------------------

To be written.  The information might be strewn about other sections,
should be gathered together here someday.

(b) Intrinsic Data Types
------------------------

    -> Tests for functionality "Evaluate core Robin Expression"

### Terms ###

Whereas an S-expression is a syntactic concept, a term is a semantic
concept.  Every term maps to an S-expression.  Most S-expressions
map to a term.  By abuse of notation, sometimes we call terms S-expressions
(but we'll try to avoid overdoing this.)

In Robin, a term is a sort of catch-all data type which includes
all the other data types.  It is inductively defined as follows:

*   A symbol is a term.
*   A boolean is a term.
*   An integer is a term.
*   A macro is a term.
*   An empty list is a term.
*   A list cell containing a term, prepended to another list
    (which may be empty), is a term.
*   An abort value is a term.
*   Nothing else is a term.

Terms have a textual representation (as S-expressions), but not all types
have values that can be directly expressed in this textual representation.
All terms have some meaning when interpeted as Robin programs, as
defined by Robin's evaluation rules, but that meaning might be to
produce an abort value as an indication that the program is in error.

### Symbol ###

A symbol is an atomic value represented by a string of characters
which may not include whitespace or parentheses or a few other
characters (TODO: decide which ones) and which may not begin with
a `#` (pound sign) or a few other characters (TODO: decide which
ones.)

When in a Robin program proper, a symbol can be bound to a value, and
in this context is it referred to as an _identifier_.  However, if an
attempt is made to evaluate a symbol which is not an identifier,
an abort value will be produced.

    | this-symbol-is-not-bound
    ? (abort (unbound-identifier this-symbol-is-not-bound))

For a symbol to appear unevaluated in a Robin program, it must be
introduced as a literal.  However, there is no intrinsic way to do this,
so in order to demonstrate it, we must use something we haven't
covered yet: a macro.  We'll just go ahead and show the example, and
will explain macros later.

    | ((macro (a e) (head a)) hello)
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

    | #t
    = #t

    | #f
    = #f

Booleans cannot be applied.

    | (#t 1 2 3)
    ? (abort (inapplicable-object #t))

### Integers ###

An integer, in the context of Robin, is always a 32-bit signed integer.
If you want larger integers or rational numbers, you'll need to build a
bigint library or such.

For example, 5 is an integer:

    | 5
    = 5

Whereas 6167172726261721 is not, and you get the 32-bit signed integer
equivalent:

    | 6167172726261721
    = -878835751

Integers always evaluate to themselves.

Integers cannot be applied.

    | (900 1 2 3)
    ? (abort (inapplicable-object 900))

### Macros ###

A macro is a term which, in an environment, describes how to
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

    | (macro (args env) args)
    = (macro (args env) args)

Macros can be applied, and that is the typical use of them.

    | ((macro (args env) args) 1)
    = (1)

### Lists ###

A list is either the empty list, or a list cell containing a value of any
type, prepended to another list.

The "head" of a list cell is the value (of any type) that it contains;
the "tail" is the other list that it is prepended to.  The empty list
has neither head nor tail.

Lists have a literal representation in Robin's S-expression based
syntax.

The empty list is notated `()` and it evaluates to itself.

    | ()
    = ()

A list with several elements is notated as a sequence of those
elements, preceded by a `(`, followed by a `)`, and delimited
by whitespace.

Non-empty lists do not evaluate to themselves; rather, they represent a macro
application.  However, the `literal` macro (whose definition is
`(macro (a e) (head a))`) may be used to obtain a literal list.

    | ((macro (a e) (head a)) (7 8)))
    = (7 8)

Lists cannot be directly applied, but since a list itself represents an
application, that application is undertaken, and the result of it can
be applied.

### Aborts ###

In Robin, errors (and other conditions which cause computation to
cease to continue) are indicated by values with a special abort type,
called abort values.  Trying to use an abort value in most operations
is an error, and consequently results in another abort value being produced.
In this manner, aborts tend to bubble up to some level of the evaluation
where they are handled.  They may either be handled explicitly by the
program with the `recover` intrinsic, or implicitly by the implementation
once they reach the outermost level of evaluation.

What the implementation does to handle an abort value that reaches the
outermost level is an implementation detail, but is generally expected
to communicate it as an error to the user somehow.  The reference
implementation of Robin produces an OS-level error code when asked to
display an abort value.  Examples of this behaviour can be found
among the error-expecting tests in this document.

An abort value contains a single value, called the payload, which
may be any Robin term.  The payload usually attempts to describe
the error (or other) condition that the abort value represents.

Abort values have a textual representation which matches an
expression that, when evaluated, evaluates to the abort value.

    | (abort 12345)
    ? (abort 12345)

(c) Conventional Data Types
---------------------------

This section lists data types that are not intrinsic, but are rather
arrangements of intrinsic types in a way that follows a convention.

### Strings ###

Strings are just lists of integers, where each integer refers to a
particular Unicode codepoint.  There is syntactic sugar for embedding
arbitrary text into a Robin Expression (see the Robin Syntax section)
and this form parses as a literal string of this type.

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

(d) Standard Environments
-------------------------

Every Robin Expression is evaluated in some kind of environment
(a mapping from symbols to the terms they are bound to.)  Robin
defines several standard environments in which expressions can be
evaluated.  Each of these environments is a superset of the others.

The smallest environment consists only of intrinsics.  The next-smallest
environment is called "small".  The largest environment is called
"stdlib", short for "standard library".  The definitions in the standard
library are split up into purpose-oriented "packages" (for example,
list functions, arithmetic functions, etc.)

### Intrinsics ###

Robin provides 15 intrinsics.  These represent the fundamental functionality
that is used to evaluate programs, and that cannot be expressed as macros
written in Robin (not without resorting to meta-circularity, at any rate.)
All other macros are built up on top of the intrinsics.

This set of intrinsics is not optional — every Robin implementation must
provide them, or it's not Robin.

One important intrinsic is `eval`.  Many macros will make use of `eval`,
to evaluate the literal args they receive.  When they do this in the
environment in which they were called, they behave a lot like functions.
But they are not obligated to; they might evaluate them in a modified
environment, or not evaluate them at all and treat them as a literal
S-expression.

Macros that are defined intrinsically does not support every operation
that defined macro support; for example, they do not support examining
their internals.  The canonical representation of an intrinsic is the name
its bound to.

    | head
    = head

All parts of the Robin Expression Language, including intrinsics,
can be passed around as values.

    | (prepend if (prepend head ()))
    = (if head)

All of the 15 intrinsics are macros, but there is nothing ontologically
requiring an intrinsic to be a value of macro type.

Each of the 15 intrinsics provided by Robin is specified in its own file
in the standard library.  Because these are intrinsics, no Robin
implementation is given for them in these files, but tests cases
which describe their behaviour are.

*   [abort](../stdlib/abort.robin)
*   [recover](../stdlib/recover.robin)
*   [equal?](../stdlib/equal-p.robin)
*   [eval](../stdlib/eval.robin)
*   [head](../stdlib/head.robin)
*   [if](../stdlib/if.robin)
*   [list?](../stdlib/list-p.robin)
*   [macro?](../stdlib/macro-p.robin)
*   [macro](../stdlib/macro.robin)
*   [number?](../stdlib/number-p.robin)
*   [prepend](../stdlib/prepend.robin)
*   [sign](../stdlib/sign.robin)
*   [subtract](../stdlib/subtract.robin)
*   [symbol?](../stdlib/symbol-p.robin)
*   [tail](../stdlib/tail.robin)

### Small ###

The "small" library represents indispensible functionality that
all but the most austere Robin programs would like to be built on.

*   [literal](../stdlib/literal.robin)
*   [list](../stdlib/list.robin)
*   [bind](../stdlib/bind.robin)
*   [env](../stdlib/env.robin)
*   [let](../stdlib/let.robin)
*   [choose](../stdlib/choose.robin)
*   [bind-args](../stdlib/bind-args.robin)

### Standard Library ###

The "standard" library represents a rich collection of functionality.
It's categorized in "packages".

*   *boolean*: and or xor not boolean?
*   *list*: empty? map fold reverse filter find append elem? length index
    take-while drop-while first rest last prefix? flatten
*   *alist*: lookup extend delete
*   *env*: env? bound? export sandbox unbind unshadow
*   *arith*: abs add > >= < <= multiply divide remainder
*   *misc*: itoa

Part 2. Robin Toplevel Language
-------------------------------

    -> Tests for functionality "Execute core Robin Toplevel Program"

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

### `assert` ###

`(assert EXPR)` evaluates the EXPR and, if the EXPR evaluates to `#f`,
or if it evaluates to an abort value, aborts processing the file.

    | (assert #t)
    = 

    | (assert 123)
    = 

    | (assert #f)
    ? (abort (assertion-failed #f))

    | (assert this-identfier-is-not-bound)
    ? unbound-identifier

### `require` ###

`(require SYMBOL)` is conceptually not different from `(assert (bound? SYMBOL))`
(see `bound?` in the stdlib for the meaning of `bound?`.)  However, since it
is given in a declarative fashion, an implementations may examine this symbol
and try to fulfill the requirement that it be bound by e.g. locating and
loading an external definition file.  Note that an implementation is not
required to do this, it is simply permitted.

    | (require if)
    = 

    | (require mumbo-jumbo)
    ? (abort (assertion-failed (bound? mumbo-jumbo)))

    | (define mumbo-jumbo 1)
    | (require mumbo-jumbo)
    = 

### `define` ###

`(define SYMBOL EXPR)` defines a global name.

    | (define true #t)
    | (display true)
    = #t

You may not try to define anything that's not a symbol.

    | (define #f #t)
    | (display #f)
    ? (abort (illegal-toplevel (define #f #t)))

You may define multiple names.

    | (define true #t)
    | (define false #f)
    | (display false)
    | (display true)
    = #f
    = #t

Names previously defined can be used in a definition.

    | (define true #t)
    | (define also-true true)
    | (display also-true)
    = #t

Names that are not yet defined cannot be used in a definition, even if
they are defined later on in the file.

    | (define also-true true)
    | (define true #t)
    | (display also-true)
    ? unbound-identifier

A name may be defined multiple times.  The meaning of this is that
several _semantically equivalent definitions_ are being given for the
name.  In this context, "semantically equivalent" means that given the
same arguments in the same environment, the two defintions will always
evaluate to the same value.

    | (define true #t)
    | (define true #t)
    | (display true)
    = #t

An implementation is allowed to check that the definitions are
semantically equivalent, and object with an error condition if it can
prove that they are not semantically equivalent.  So, for example, the
following is allowed to be considered an error:

    (define true #t)
    (define true #f)

An implementation should not, however, object with an error condition
if it cannot prove the semantic inequivalence (although it is certainly
free to produce a warning in this case).  A good example of this would
perhaps be a definition of a function that goes through a Collatz
sequence and evaluates to `#t`, and a function that simply always
evaluates to `#t`.

An implementation is also allowed to simply take it on faith that
the definitions are semantically equivalent.  (The following example
is perhaps not the best example.)

    | (define true #t)
    | (define true ((macro (args env) #t)))
    | (display true)
    = #t

If they are not genuinely equivalent, of course, that is a programmer
error like any programmer error — the semantics of Robin's `define`
do not excuse the programmer from exercising their own diligence.

Since the definitions are supposed to be equivalent, which definition
the implementation chooses, is ultimately up to the implementation.
The implementation could even choose to use different definitions in
different places.  However, the current convention is that the
definition that is generally preferred because it is the most efficient
will be given first (perhaps as a built-in provided by the implementation),
so implementations would do well to support choosing the first definition
for each symbol that has multiple definitions.

### `reactor` ###

`(reactor LIST-OF-SYMBOLS STATE-EXPR BODY-EXPR)` installs a reactor.  Reactors
permit the construction of Robin programs that interact with their environment.
See the [Reactors](#reactors) section for more information on reactors.

Part 3. Robin Reactors
----------------------

    -> Tests for functionality "Execute core Robin Toplevel Program"

To separate the concerns of computation and interaction, Robin provides
a construct called a _reactor_.  While evaluation of a Robin expression
accomplishes side-effect-free computation, reactors permit the construction
of event-driven programs which may interact with a user, a remote server on
a network, or other source of events, while they are executing.  Reactors
are similar to event handlers in Javascript, or to processes in Erlang.

In Robin, a reactor is installed by giving a top-level form with the
following syntax:

    (reactor SUBSCRIPTIONS INITIAL-STATE-EXPR TRANSDUCER)

The first argument of the `reactor` form is a literal (unevaluated) list
of symbols, called the _subscriptions_ for the reactor.  Each symbol names
a _facility_ with which the reactor wishes to be able to interact.

The second argument is evaluated, and becomes the _initial state_ of the
reactor.

The third argument of the `reactor` form is evaluated to obtain a
macro.  This is called the _transducer_ of the reactor.

Whenever an event of interest to the reactor occurs, the transducer is
evaluated, being passed two (pre-evaluated) arguments:

*   A two-element list called the _event_.  The elements are:
    *   A literal symbol called the _event type_, specifying what kind of event
        happened.
    *   An arbitrary value called the _event payload_ containing more data about
        the event, in a format specific to the type of event.
*   The current state of the reactor.  (This will be the initial state
    if the reactor body has never before been evaluated.)

Given these things, the transducer is expected to evaluate to a list where the
first element is the new state of the reactor, and each of the subsequent
elements is an _command_, which is itself a two-element list containing:

*   A literal symbol called the _command type_ specifying the kind of
    command that is being requested to be executed; and
*   An arbitrary value called the _command payload_ containing more data
    about the command, in a format specific to that type of command.

There may of course be zero commands in the returned list, but it is an
error if the returned value is not a list containing at least one element.

If the transducer evaluates to an abort value, no commands will be executed,
and the state of the transducer will remain unchanged.  Implementations
should allow such occurrences to be visible and/or logged.

In fact, commands _are_ events.  We just call them commands when it is
a reactor producing them, and events when a reactor is receiving them.

### Standard Events ###

#### `init` ####

When a reactor first starts up it will receive an event telling it that
it has started up.  The event type for this event is the literal symbol
`init`.

There are two things a reactor will almost always want to do when it
receives an `init` event: establish a known initial state, and
subscribe to facilities.

It establishes a known initial state by returning an initial state
value as the first element of the list it returns.

It subscribes to facilities by returning commands which request
subscription to those facilities.  Once subscribed, it will receive
`subscribed` events from them.  If the facility is not available, it
will receive a `not-available` event.  It may then elect to abort,
or choose an alternate facility, or so forth.

### Standard Commands ###

#### `stop` ####

Stops the current reactor, and removes it from the list of active
reactors.  It will no longer receive any events.

### Standard Facilities ###

If a reactor isn't subscribed to any facilities, it won't necessarily
receive any events, although this is implementation-specific.

The set of facilities is expected to be largely implementation-specific.

All the same, there will probably be a set of standard facilities.

Let's describe one such facility for concreteness.

#### `line-terminal` ####

    -> Tests for functionality "Execute Robin Toplevel Program (with Small)"

The `line-terminal` facility allows a Robin program to interact with
something or someone over a line-oriented protocol, similar to what
"standard I/O" routed to a terminal gets you under Unix.  Note that
this is not guaranteed to be the "real" standard I/O; it could well be
simulated with modal dialogue boxes in a GUI, or with textareas on a web
page under Javascript, or so forth.

The `line-terminal` facility understands commands of the form

    (writeln STRING)

The `STRING` argument should be a Robin string (list of integers).  Those
integers, as bytes, are sent to whetever is listening on the other end of
the line terminal.  When attached to an actual terminal console (whether real
or emulated), this would typically cause an ASCII representation of those bytes
to be displayed.

It also understands

    (write STRING)

which will write the STRING but not terminate the line.

Knowing this, we can write a "Hello, world!" program.  To keep it
simple, we'll simply assume the line-terminal facility exists.
We won't bother to subscribe to it.  In addition, note that this reactor
essentially doesn't keep any state — the initial state of the reactor is simply
the integer 0, and the state is set to 0 after each event is reacted to.

    | (reactor (line-terminal) 0
    |   (macro (args env)
    |     (bind event (head args)
    |       (bind event-type (head event)
    |         (if (equal? event-type (literal init))
    |           (list 0
    |             (list (literal writeln) (literal ''Hello, world!''))
    |             (list (literal stop) 0))
    |           (list 0))))))
    = Hello, world!

Reactors which interact with `line-terminal` receive `readln` events.

`readln`, is sent when a line of text is received
on the "standard input".  The payload for this event is a Robin string
of the line of text received.  This string does not contain any end-of-line
marker characters.

Thus we can construct a simple `cat` program:

    | (reactor (line-terminal) 0
    |   (macro (args env)
    |     (bind event (head args)
    |       (bind event-type (head event)
    |         (bind event-payload (head (tail event))
    |           (if (equal? event-type (literal readln))
    |             (list 0
    |               (list (literal writeln) event-payload))
    |             (list 0)))))))
    + Cat
    + Dog
    = Cat
    = Dog

#### `random-u16-source` ####

The `random-u16-source` facility allows a Robin program to request,
and obtain, unsigned 16-bit numbers whose value is (ideally speaking)
unpredictable.

The `random-u16-source` facility understands commands of the form

    (obtain-random-u16 0)

In response to one of these commands, this facility generates
an event of the form

    (random-u16 NUMBER)

where NUMBER is a random number between 0 and 65535.

### General Reactor properties ###

A reactor can issue multiple commands in its response to an event.

    | (reactor (line-terminal) 0
    |   (macro (args env)
    |     (bind event (head args)
    |       (bind event-type (head event)
    |         (bind event-payload (head (tail event))
    |           (if (equal? event-type (literal readln))
    |             (list 0
    |               (list (literal writeln) (literal ''Line:''))
    |               (list (literal writeln) event-payload))
    |             (list 0)))))))
    + Cat
    + Dog
    = Line:
    = Cat
    = Line:
    = Dog

When receiving a malformed command, a facility may produce a warning
message of some kind, but it should otherwise ignore it and keep going.

    | (reactor (line-terminal) 0
    |   (macro (args env)
    |     (bind event (head args)
    |       (bind event-type (head event)
    |         (bind event-payload (head (tail event))
    |           (if (equal? event-type (literal readln))
    |             (list 0
    |               (literal what-is-this)
    |               (literal i-dont-even)
    |               (list (literal writeln) event-payload))
    |             (list 0)))))))
    + Cat
    + Dog
    = Cat
    = Dog

If evaluating the transducer of a reactor returns an abort value, the reactor
remains in the same state and issues no commands, but always recovers
so that it can continue to handle subsequent events (i.e. it does not crash).

An implementation is encouraged to allow these to be logged (and the
reference implementation will display them if `--show-events` is given)
but this is not a strict requirement.

    | (reactor (line-terminal) 0
    |   (macro (args env)
    |     (bind event (head args)
    |       (bind event-type (head event)
    |         (bind event-payload (head (tail event))
    |           (if (equal? event-type (literal readln))
    |             (if (equal? (head event-payload) 65)
    |               (abort 999999)
    |               (list 0 (list (literal writeln) event-payload)))
    |             (list 0)))))))
    + Cat
    + Dog
    + Alligator
    + Bear
    = Cat
    = Dog
    = Bear

Reactors can keep state.

    | (define inc (macro (args env)
    |               (subtract (eval env (head args)) (subtract 0 1))))
    | (reactor (line-terminal) 65
    |   (macro (args env)
    |     (bind state (head (tail args))
    |       (bind event (head args)
    |         (bind event-type (head event)
    |           (bind event-payload (head (tail event))
    |             (if (equal? event-type (literal readln))
    |               (list (inc state) (list (literal writeln) (list state)))
    |               (list state))))))))
    + Cat
    + Dog
    + Giraffe
    = A
    = B
    = C

Multiple reactors can be instantiated, will react to the same events.
Note that reactors react in the *opposite* order they were installed.

    | (define inc (macro (args env)
    |               (subtract (eval env (head args)) (subtract 0 1))))
    | (reactor (line-terminal) 65
    |   (macro (args env)
    |     (bind state (head (tail args))
    |       (bind event (head args)
    |         (bind event-type (head event)
    |           (bind event-payload (head (tail event))
    |             (if (equal? event-type (literal readln))
    |               (list (inc state) (list (literal writeln) (list state)))
    |               (list state))))))))
    | (reactor (line-terminal) 0
    |   (macro (args env)
    |     (bind event (head args)
    |       (bind event-type (head event)
    |         (bind event-payload (head (tail event))
    |           (if (equal? event-type (literal readln))
    |             (list 0
    |               (list (literal writeln) event-payload))
    |             (list 0)))))))
    + Cat
    + Dog
    + Giraffe
    = Cat
    = A
    = Dog
    = B
    = Giraffe
    = C

A reactor can stop by issuing a `stop` command.

    | (define inc (macro (args env)
    |               (subtract (eval env (head args)) (subtract 0 1))))
    | (reactor (line-terminal) 65
    |   (macro (args env)
    |     (bind state (head (tail args))
    |       (bind event (head args)
    |         (bind event-type (head event)
    |           (bind event-payload (head (tail event))
    |             (if (equal? event-type (literal readln))
    |               (if (equal? state 68)
    |                 (list state (list (literal stop) 0))
    |                 (list (inc state) (list (literal writeln) event-payload)))
    |               (list state))))))))
    + Cat
    + Dog
    + Giraffe
    + Penguin
    + Alligator
    = Cat
    = Dog
    = Giraffe

Stopping one reactor does not stop others.

    | (define inc (macro (args env)
    |               (subtract (eval env (head args)) (subtract 0 1))))
    | (reactor (line-terminal) 65
    |   (macro (args env)
    |     (bind state (head (tail args))
    |       (bind event (head args)
    |         (bind event-type (head event)
    |           (bind event-payload (head (tail event))
    |             (if (equal? event-type (literal readln))
    |               (if (equal? state 68)
    |                 (list state (list (literal stop) 0))
    |                 (list (inc state) (list (literal writeln) event-payload)))
    |               (list state))))))))
    | (reactor (line-terminal) 65
    |   (macro (args env)
    |     (bind state (head (tail args))
    |       (bind event (head args)
    |         (bind event-type (head event)
    |           (bind event-payload (head (tail event))
    |             (if (equal? event-type (literal readln))
    |               (list (inc state) (list (literal writeln) (list state)))
    |               (list state))))))))
    + Cat
    + Dog
    + Giraffe
    + Penguin
    + Alligator
    = A
    = Cat
    = B
    = Dog
    = C
    = Giraffe
    = D
    = E

### Subscribing and unsubscribing to facilities ###

TODO.

Listing a facility in the SUBSCRIPTIONS of the reactor isn't the
only way for a reactor to be notified of events from the facility.
The reactor can also subscribe to the facility at some point after the
reactor has started, and later even unsubscribe from it as well.

### Communicating between reactors ###

It is not really recommended to implement a system with multiple
reactors.  It is better to compose a single large reactor out of
multiple macros.

But currently we allow it, so we should say some words about it.

When a reactor issues a command, all other reactors see it as an
event.
