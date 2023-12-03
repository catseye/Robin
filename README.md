Robin
=====

_Version 0.8_ | _Try it online_ [@ catseye.tc](https://catseye.tc/installation/Robin)
| _See also:_ [Pixley](https://codeberg.org/catseye/Pixley#pixley)

- - - -

Overview
--------

**Robin** is an [excessively principled](doc/Rationale.md)
and [thoroughly specified](doc/Robin.md) functional programming language with
[eager evaluation, latent typing, and a homoiconic syntax](#scheme),
based on a [radically simple core semantics](#forth) in which
[the so-called "fexpr" is the fundamental abstraction](#picolisp)
and both functions and macros are defined in terms of it.

Expressions in Robin are [referentially transparent](#haskell); programs
interact with the outside world [through an event-driven framework](#elm).

For more information, see the [extended description](#extended-description)
below.

Quick Start
-----------

The Robin reference interpreter is written in about 1300 lines of Haskell.
To use it, you'll need an implementation of Haskell installed (typically either
`ghc` or Hugs).

First, clone this repository and `cd` into the repo directory.  Then run

    make

If you have `cabal` installed, the Makefile will use it to build the `robin`
executable, and this will take care of obtaining and building the dependencies.

If you do not have `cabal`, the Makefile will use `ghc` directly to build the
executable, but in this case, you will need to ensure you have dependencies
like `parsec` and `random` installed, yourself.

(If you don't have `ghc` at all, no executable will be built; but that's OK,
because in this case the `bin/robin` driver script will fall back to using
`runhaskell` or `runhugs` instead.)

In any case, the Makefile will also build build the standard library
(`pkg/stdlib.robin`).  And this same Makefile can be used to build the
JavaScript version of the interpreter, with `make web`.

After running `make`, you can run the Robin interpreter using the
driver script in `bin`, on one of the example Robin sources in `eg` like so:

    bin/robin pkg/stdlib.robin eg/hello-world.robin

You should see

    Hello, world!

To continue learning to program in Robin you can follow
[The Robin Tutorial](doc/Tutorial.md).

Testing
-------

If you have a few minutes to spare, and you have [Falderal][] installed,
you can run the test suite (consisting of more than 600 unit tests) by running

    ./test.sh

The tests that use only Robin's core semantics (with no help from implementation
"builtins") are quite slow, so you may want to skip them, by running

    APPLIANCES="appliances/robin.md" ./test.sh

The test suite will also run some property tests (using QuickCheck).  Notably,
for every operator that is defined multiple times (which includes much of stdlib,
where the core definitions are written in Robin but also implemented in Haskell
as "builtins" in the reference interpreter), QuickCheck will attempt to falsify
the assertion that the definitions define the same operator.  These attempts are
currently rather crude; there is lots of room for improvement for them in some
future release.

Extended Description
--------------------

For experienced programmers, Robin might be best described by listing
the languages that have had the strongest influences on it:

### Scheme ###

Like [Scheme][], Robin is eagerly evaluated, latently typed, and homoiconic,
as well as properly tail-recursive and lexically scoped (at least by default),
and tries hard to be well-defined and system-agnostic, but (as you can read
below) diverges significantly from Scheme in other ways.

### Forth ###

Like [Forth][], Robin has a radically simple core semantics.  There are 15
intrinsic operations; every symbol in the standard library is defined in terms
of these intrinsics, while an implementation is free to provide its own
(perhaps more efficient) implementation of any such symbol.  (See also
[Pixley][]).

### PicoLisp ###

In most languages, the arguments to a function are evaluated before the
function is applied, but [PicoLisp][] allows defining functions with
unevaluated arguments.  In historical Lisp, such operators were called
[fexpr][]s.  Robin adopts fexprs as the fundamental abstraction — both
functions and macros are defined in terms of fexprs.

The [Kernel][] programming language also takes fexprs as its fundamental
abstraction; however, Robin was developed oblivious of Kernel — it adapted
the idea directly from PicoLisp.

### Haskell ###

Like [Haskell][], Robin is referentially transparent (often described as
"purely functional") — mutation of values is forbidden.  (Robin intentionally
does not, however, adopt lazy evaluation or a static type system.)

### Elm ###

Interactive programs in Robin are built by composing transducers which are driven
by events and produce effects (which are modelled as further events), in a
manner very similar to [The Elm Architecture][].

### Bourne shell ###

Arbitrary text can by embedded in a Robin program using a syntax
very much like a ["heredoc"](https://en.wikipedia.org/wiki/Here_document),
except it is an S-expression.

### English ###

Deserves at least a passing mention here, as one thing that Robin
discards from Scheme is its jargony terminology: no `cdr`, no `cons`,
no `lambda`.  (A notable exception is `fexpr` simply because there is no
satisfying short, non-jargony word that connotes how these operators work.)

For a full description of the Robin language, see
[the Robin specification document](doc/Robin.md).

Repository Layout
-----------------

*   `appliances/` — test appliances for the literate test suite.
*   `bin/` — driver script, destination for executable when built.
*   `demo/` — contains HTML5 document demonstrating build to JS by Haste.
*   [`doc/`](doc/README.md) — Tutorial, specification, rationale, etc.
*   `eg/` — example programs written in Robin
*   `src/` — Haskell source for reference interpreter.
*   `stdlib/` — normative definitions of standard library symbols.
*   [`HISTORY.md`](HISTORY.md) — history of this distribution.
*   [`TODO.md`](TODO.md) — plans.

[Scheme]:    http://schemers.org/
[Haskell]:   https://www.haskell.org/
[fexpr]:     https://en.wikipedia.org/wiki/Fexpr
[PicoLisp]:  http://picolisp.com/
[Kernel]:    http://web.cs.wpi.edu/~jshutt/kernel.html
[Forth]:     https://en.wikipedia.org/wiki/Forth_(programming_language)
[Pixley]:    https://catseye.tc/node/Pixley
[Elm]:       https://elm-lang.org/
[The Elm Architecture]: https://guide.elm-lang.org/architecture/
[shelf]:     https://catseye.tc/node/shelf
[Falderal]:  https://catseye.tc/node/Falderal
