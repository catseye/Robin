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
lambdas -- they do not automatically evaluate their arguments.  Function
values are built on top of macros, using the built-in macro `eval`.

Like Erlang, Robin is purely functional except for message-passing.
That is, functions have no side-effects, with the single exception of
being able to send messages to, and receive messages from, other processes.
All facilities of the underlying system are modelled as such processes.

Robin supports a simple system of raising and handling exceptions.  This
helps define the semantics of otherwise undefined operations, such as trying
to obtain the tail of a non-pair.

Lastly, Robin unifies (to a degree) programming and static analysis.  The
language itself defines essentially no rules of static correctness beyond
the basic rules about syntax.  Static analyses are available in modules,
just like any other kind of functionality, letting the programmer choose
what level of pre-execution checking is applied to their code.

[Erlang]:   http://erlang.org/
[PicoLisp]: http://picolisp.com/
[Pixley]:   http://catseye.tc/projects/pixley/
[Scheme]:   http://schemers.org/

Distribution
------------

The current version of Robin under development is version 0.1.  Even it
is unreleased, so what you're looking at here is pure "technology
preview" stuff.  Expect everything to change, perhaps drastically.

Installation
------------

Step 1: Obtain the sources.

    $ hg clone https://bitbucket.org/catseye/robin

*or*

    $ git clone git://github.com/catseye/Robin.git

Step 2: Make sure you have `ghc`, and the Haskell packages `parsec` and
(optionally) `hscurses` installed (these can both be instaled via `cabal`).
The following instructions are for Ubuntu; equivalents for other operating
systems are left as an exercise for the reader.

    $ sudo apt-get install ghc cabal-install
    $ cabal install parsec
    $ cabal install hscurses   # if you want to use the console module

Step 3: Build the sources.

    $ cd robin
    $ ./build.sh

All built-in modules are built by default.  If you want to exclude some
modules (for example `console`), you can list them in the `WITHOUT`
environment variable.  For example,

    $ WITHOUT="CrudeIO Console" ./build.sh

Note that if you exclude the built-in `small` module, `robin` will fall back
to the `small` module written in Robin, but expect it to be *much* slower.

Step 4: Get Falderal, so that you can run the tests.

    $ cd ..
    $ hg clone https://bitbucket.org/catseye/falderal
    $ cd falderal
    $ ./install.sh
    $ cd ..

Step 5: Run the tests.

    $ cd robin
    $ ./test.sh

Installation of the compiled `robin` executable so that it can be run from
any working directory isn't supported yet; you'll need to be in your clone's
root directory to run `bin/robin` for now.

It is possible to build `robin` under Windows, using `ghc` from the Haskell
Platform for Windows, and Cygwin to run the shell scripts; however, there
are various minor considerations which are currently outside the scope of
this README.  If you're really motivated, you'll figure it out.

Documentation
-------------

Robin's fundamental semantics are documented in
[doc/Robin.falderal](doc/Robin.falderal).  From there you will find links
to documentation on each of the standard modules as well.

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

### Fundamental Semantics ###

* Add an opaque type -- opaque values have internals that can only be
  accessed inside the module in which they were created.  Actually, we
  already have function values, and they're traditionally opaque; but
  I'm not sure that solves the problem of them only being accessible
  from the module in which they're defined.

* Remove improper lists from the language.  This is starting to look
  like the more attractive route.

### Standard Modules ###

* In the `concurrency` module, finalize the semantics for exception and
  final-result and unknown-tag reply messages, particularly during `call`
  and `respond`.

* Establish (and enforce) conventions listed in the Style document.

* Write a `timer` module which exports a process which can be asked
  (via a message) to send back a message after a given time has passed.
  This could be used to build a version of `recv` which can time out.

* Enhance the `console` module.  Write a version of `robotfindskitten`
  using it.

* In the `arith` module, make `sum` and `product` that work on lists.
  Possibly make `product` short-circuiting.  Possibly add `int-pow`.

* A macro for asserting that the correct number of arguments have been
  given to a macro.  (Right now the `small` macros don't complain if
  given too many arguments.)

* Some kind of macro for capturing the recursive function call pattern
  (like `letrec`, but not necessary to support mutual recursion.)  Possibly
  called `bind-recur`.  Also `let-recur` could build on that.  Turn:

    (bind-recur foo (fun (a b c)
                      (if a
                        (b c)
                        (foo (bar a) b c))) ...)

into

    (bind foo
      (bind foo-r (fun (self a b c)
                    (if a
                      (b c)
                      (self self (bar a) b c)))
        (fun (a b c) (foo-r foo-r a b c))) ...)

Lack of a `gensym` will make this tricky.  We don't really have to
bind `foo-r`, we can just repeat the definition of the recursive
function; but I don't know how we can add the `self` parameter without
potentially shadowing a user parameter also named `self`.

An alternative which might be easier (but less elegant) would be to
introduce a primitive `(self)` which evaluates to the function currently
being evaluated.  This might make the `self` parameter to macros redundant,
though.

* Use `subst-env` to implement `literal-with`, as a substitute for
  `quasiquote`, which works more like `let` (cf. `let-symbol`).  Also
  possibly `quasi-literal` which works more like Perl's embedded `$`
  variables.  (Extending this to embedded expressions is also possible.)

* Work out the static analysis modules.  See the Static Analysis document
  for more information.

* Either extend the `exception` module and semantics, or create a new
  module for exception semantics extended as follows.  Allow the backtrace
  of an exception to be accessed as a Robin object, and, to some extent,
  manipulated.  When an exception is raised in a context where another
  exception is being caught, allow the backtraces to be chained together.
  When an exception is raised during (say) the reading of a text file,
  allow the backtrace to be amended with the position within the text
  file to which the problem can be traced.  The purpose of all this is
  to allow producing more complete error messages at the top level.

### Possible Future Modules ###

* Write a `functional` module which exports some functions for working
  with functions, such as `identity`, `compose`, and possibly `curry`
  and `uncurry`.

* Possibly make a `transcendental` module to contain `exp`, `pow`,
  `log`, `sqrt`, and so forth.

* Write a `trig` module which exports trigonometric functions `cos`,
  `sin`, `tan`, `atan`, `pi`, etc.  Initially write this in Robin,
  but it's a good candidate for implementing natively.

* Write a `set` module which exports functions which treat lists as
  sets, with each operation ensuring the set elements are unique in
  the list.

* Write a `pixley` module which exports only the identifiers supported
  by Pixley.  This could be imported, instead of `core`, to emulate
  Pixley in Robin.

### Documentation ###

* Document the `boolean` module.

* Document the `arith` module.

* Document the alist functions in the `list` module.

* Document the literate Haskell implementation better -- right now it's
  pretty scant.

* Document the evaluation rules (they're very similar to Scheme's, but
  they should still be written down.)

### Tests ###

* Fuller tests for `call`.

* Make the tests for `core` only ever import the `core` module -- rewrite
  those tests which currently import `small`, although they may be pretty
  ugly expressed purely in `core` terms.

* Have the test runner know to only test those built-in modules which were
  selected for inclusion during the build step.  (Write those options to
  a text file which the test runner script reads.)

* Informally test tail-recursive behavior (does an infinite loop
  leak memory?)

### Reference Implementation ###

* Allow the `robin` executable to be installed on your `PATH`, and let it be
  configured to understand how to find modules for loading, probably by
  requiring a "module path" passed on the command line.  (You would 
  probably typically write a little wrapper script, or alias, that sets
  this to where-ever you keep your modules; in which case, the executable
  itself doesn't even need to be on your `PATH`.)

* Remove execution trace facility.

### Other Implementations ###

* Build another implementation of Robin.  This should probably wait,
  as even the fundamental semantics are still a moving target, and
  having to maintain two implementations is not so desirable.  However,
  this will let me have a place to implement "practical" things that
  arguably don't belong in the reference implementation.

* Allow the implementation to use a configuration file (likely a `.robinrc`
  file (or directory) in the user's home directory) to specify which files
  (and where) to load for which modules.

* Implement a selective execution trace facility (configured in the
  configuration file, probably) which starts and stops tracing at
  configured points during program execution.

* Upon an uncaught exception, dump a backtrace.  This should be based
  on the current continuation at the time the exception was raised.
