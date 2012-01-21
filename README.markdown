Robin
=====

Robin is a programming language which draws from [Scheme][] (via [Pixley][]),
[PicoLisp][], and [Erlang][].

Robin's core language is quite ascetic; however, Robin supports a module
system, by which functionality can be brought in from modules (written in
Robin, or some other language,) which expose their functionality to programs
and other modules.

The standard modules include a small "standard library" to make programming
slightly easier, a module for concurrent processes with message-passing, and
a module for handling exceptions.

Robin programs are homoiconic, and presented in a S-expression-based syntax.

Instead of function values, Robin supplies _macros_ as the primitive
abstraction.  Robin's macros are somewhat like PicoLisp's one-argument
lambas -- they do not automatically evaluate their arguments.  Function
values are built on top of macros, using the built-in macro `eval`.

Like Erlang, Robin is purely functional except for message-passing.
That is, functions have no side-effects, with the single exception of
being able to send messages to, and receive messages from, other processes.
All facilities of the operating system are modelled as such processes.

Lastly, Robin supports a simple system of raising and handling exceptions.
This helps define the semantics of otherwise undefined operations, such as
trying to obtain the tail of a non-pair.

[Erlang]:   http://erlang.org/
[PicoLisp]: http://picolisp.com/
[Pixley]:   http://catseye.tc/projects/pixley/
[Scheme]:   http://schemers.org/

Distribution
------------

The current version of Robin under development is version 0.1.  Even it
is unreleased, so what you're looking at here is pure "technology
preview" stuff.  Expect everything to change, perhaps drastically.

Documentation
-------------

Robin's fundamental semantics are documented in
[doc/module/Robin.falderal](doc/module/Robin.falderal).  From there you
will find links to documentation on each of the standard modules as well.

Goals
-----

* To not be unduly burdensome to implement or analyze.  The core language
  is kept very small, the "standard library" can be written in Robin itself,
  and features such as concurrency and exceptions are optional.  The core
  language is purely functional, to keep it mathematically simple, and the
  reference implementation is in Haskell, which is a lot closer to an
  "executable semantics" than one in, say, C would be.  The functionality
  of the language is thoroughly tested.  Both the implementation and the
  test suite are written in a literate style, to keep the prose of the
  specification in close proximity to the code so that they can be easily
  checked against each other for inconsistencies.

* To err on the side of beauty and simplicity and orthogonality, rather
  than efficient implementation or expediency.

* At the same time, to allow the programmer to do "real" work, like
  interfacing with an actual computer.

* At the same time as that, to be decoupled from any particular computer
  or operating system, as far as possible.  The language does not specify
  how Robin programs should be run, nor how to locate modules that are
  imported.  Devices are abstracted to "virtual devices" and are modelled
  as processes; input and output are done with message-passing.

* To minimize atavisms and jargon.  The legacy of Robin's lexicon is
  Scheme, which itself comes from the legacy of Lisp; while there are some
  good patterns here (like predicates whose names end in `?`), there are
  also a lot of anachronisms (like `car` and `cdr`) which should be
  jettisoned.  Proper English words should be used instead, although of
  course there is room for abbreviations when they are unambiguous
  (`env`, `eval`, `expr`, `arith`, and so forth.)

* To serve as an outlet for my predilictions.  Sometimes, when using a
  language, you come across a feature or aspect that just strikes you
  as wrong-headed, and it makes you want to build something that doesn't
  irritate you as badly.  Robin is, to some extent, that, for me.

* To not be taken *too* seriously -- it has many of the attributes of a
  production language, but it *is* something I am undertaking for fun.

Plans
-----

* Establish the relationship between exceptions and processes -- if a
  process raises an exception that it does not catch, it should send
  a message to its parent (the process that spawned it.)

* Fuller tests for exceptions.

* Add an opaque type -- opaque values have internals that can only be
  accessed inside the module in which they were created.

* Add comments to the language.

* Add sugar for strings to the language (internally they would just be
  lists of numbers, which are Unicode code points.)

* Add decimal and proper-fractional sugar for rational numbers, i.e.
  `2-1/5` and `2.2`.

* Add richer concurrency primitives (`call` and `respond`, which assume
  the message consists of an envelope (containing the caller's pid), a
  "tag" symbol, and a payload; `respond` would be like a case statement
  against the tags, and would know which pid to reply to.)

* Document the `crude-io` module, and add tests for it.

* Document the `list` module.  Add `take-while`, `drop-while`, `first`,
  `rest`, `last`, and `flatten` to it.  Possibly add alist functions to
  it, or create a new `alist` module for that purpose.

* Document the `env` module, and add some more macros to it,
  particularly `binding-for` and `unshadow`.

* Write a `random` module which exports a process which can be asked
  (via a message) to send back a random number in a given range.

* Write a `timer` module which exports a process which can be asked
  (via a message) to send back a message after a given time has passed.
  This could be used to build a version of `recv` which can time out.

* Write a `pixley` module which exports only the identifiers supported
  by Pixley.  This could be imported, instead of `core`, to emulate
  Pixley in Robin.

* Write a `boolean` module which exports the basic set of Boolean
  operators: `and`, `or`, `not`, `xor`, and maybe `impl`.  Maybe make
  `and` `or` and `xor` take any number of arguments.

* Write a `functional` module which exports some functions for working
  with functions, such as `identity`, `compose`, and possibly `curry`
  and `uncurry`.

* Write an `arith` module which exports the basic set of arithmetic
  operators: `-` and `/`, `+` and `*` (taking any number of arguments),
  `mod`, `>`, `<`, `>=`, `<=`, `=`, `/=` or `!=`, and possibly `exp`,
  `pow`, `log`, and `sqrt`.

* Write a `trig` module which exports trigonometric functions `cos`,
  `sin`, `tan`, `atan`, `pi`, etc.  Initially write this in Pixley,
  but it's a good candidate for implementing natively.

* Write a `set` module which exports functions which treat lists as
  sets, with each operation ensuring the set elements are unique in
  the list.

* A macro for asserting that the correct number of arguments have been
  given to a macro.  (Right now the `small` macros don't complain if
  given too many arguments.)

* Some kind of macro for capturing the recursive function call pattern
  (probably `letrec`, but not necessary to support mutual recursion.)

* Document the "why" behind some of the design decisions.

* Document some style guidelines.

* Document the literate Haskell implementation better -- right now it's
  pretty scant.

* Document the evaluation rules (they're very similar to Scheme's, but
  they should still be written down.)

* Make the tests for `core` only ever import the `core` module -- rewrite
  those tests which currently import `small`, although they may be pretty
  ugly expressed purely in `core` terms.

* Support qualifiers during module import.  Have identifiers be imported
  from modules qualified by default, and have something to turn this off.
  Possibly support "only" and "hiding" qualifiers.

* Refactor module-handling code; put it in its own module, and add a
  module cache (not just for performance, but also to ensure that objects
  created by modules, e.g. processes, are singletons.)

* Add a "trace" flag to IEnv and a `-t` flag to the implementation, to
  trace the execution of a running Robin program.

* Write a test adapter that allows modules to be specified inside
  Falderal tests (before a line that says "cut here").

* Informally test tail-recursive behavior (does an infinite loop
  leak memory?)

* Write _Hunt the Wumpus_ in Robin!
