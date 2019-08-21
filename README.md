Robin
=====

_Version 0.3.  Work-in-progress, subject to change._

**Robin** is a programming language in the general vein of
[Scheme][], but which diverges significantly from Scheme in several
respects, taking inspiration from other languages:

*   [Haskell][]: Purely functional; mutation of values is forbidden.
*   [Pixley][]: A radically simple core semantics with a fairly rich
    standard library built on top of those semantics.
*   [PicoLisp][]: The macro (rather than the function) is the fundamental
    abstraction. There is a `lambda`, but it's defined as a macro!
*   [Elm][]: Reactive programs are built by composing transducers
    which listen for events and produce effects.

For more information, see [Overview of Robin](#overview-of-robin) below.

Quick Start
-----------

You'll need an implementation of Haskell installed (typically either `ghc`
or Hugs).

If you have [shelf][] installed, you can just run `shelf_dockgh catseye/Robin`.

If not, you can clone this repository, `cd` into the repo directory, and run

    ./build.sh

to build the reference interpreter.  You can then run it on one of the
example Robin sources in `eg` like so:

    bin/robin eg/hello-world.robin

If you don't have `ghc`, no executable will be built when you run `./build.sh`,
but the wrapper script will still work (it will use `runhaskell` or `runhugs`.)

Testing
-------

If you have a few minutes to spare, and you have [Falderal][] installed,
you can run the test suite by running

    ./test.sh

The tests that use only Robin's core semantics (`--no-builtins` flag) are quite
slow, so you may want to skip them.  You can skip them by running

    APPLIANCES="appliances/robin.md" ./test.sh

Overview of Robin
-----------------

Robin is a homoiconic S-expression-based language (similar to, for example,
[Scheme][], with influences from [Pixley][] and [PicoLisp][]) with the
following features:

*   The _macro_ (rather than the function) as the fundamental abstraction
    mechanism.  There is a function form, but it's defined as a macro!
*   A very small set of intrinsic operations.
*   A very small reference implementation in Literate Haskell
    (about 600 lines of code, excluding the explanatory prose.)
*   A fairly rich standard library of macros built on top of those intrinsic
    operations.  An implementation can choose to implement standard library
    functions directly for better performance, or can choose to implement
    only the intrinsic operations, for simplicity.
*   A fairly rich test suite (about 460 test cases.)
*   An almost zealous system-agnosticism.
*   An almost zealous disdain for escape characters.  Robin's string syntax
    never needs them (it's more like a lightweight "heredoc".)
*   A module system (which is rather fast-and-loose, so it's perhaps not
    fair to call it a module system.  It's more like C's `#include`s —
    except it's zealously system-agnostic.  And actually we're still working
    out the details here.  See the file [doc/Modules.md](doc/Modules.md).)
*   A(n attempt at) a clean separation of evaluation (no "side-effects") and
    execution (with "side-effects" and system interaction) by the use of
    _reactors_ (which are basically event handlers.)  See the file
    [doc/Reactor.md](doc/Reactor.md) for more information.

### Documentation ###

Robin's fundamental semantics are documented in [doc/Robin.md](doc/Robin.md).

History of Robin can be found in [HISTORY.md](HISTORY.md).

[Scheme]:    http://schemers.org/
[Haskell]:   https://www.haskell.org/
[PicoLisp]:  http://picolisp.com/
[Pixley]:    https://catseye.tc/node/Pixley
[Elm]:       https://elm-lang.org/
[shelf]:     https://catseye.tc/node/shelf
[Falderal]:  https://catseye.tc/node/Falderal
