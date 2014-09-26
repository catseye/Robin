Robin
=====

_This is a work in progress_

Robin is a homoiconic S-expression-based language (similar to, for example,
[Scheme][], with influences from [Pixley][] and [PicoLisp][]) with the
following features:

*   The _macro_ (rather than the function) as the fundamental abstraction
    mechanism.  There is a function form, but it's defined as a macro!
*   A very small set of built-in operations.
*   A very small reference implementation in Literate Haskell
    (about 600 lines of code, excluding the explanatory prose.)
*   A fairly rich standard library of macros built on top of those built-in
    operations.  (Thus it can be used as either a "low-level" or "high-level"
    language.)
*   A fairly rich test suite (about 460 test cases.)
*   An almost zealous system-agnosticism.
*   An almost zealous disdain for escape characters.  Robin's string syntax
    never needs them (it's more like a lightweight "heredoc".)
*   A module system (which is rather fast-and-loose, so it's perhaps not
    fair to call it a module system.  It's more like C's `#include`s —
    except it's zealously system-agnostic.  And actually we're still working
    out the details here.  See the file `doc/Modules.markdown`.)
*   A(n attempt at) a clean separation of evaluation (no "side-effects") and
    execution (with "side-effects" and system interaction) by the use of
    _reactors_ (which are basically event handlers.)  See the file
    `doc/Reactor.markdown` for more information.

Quick Start
-----------

You'll need either `ghc` or Hugs installed.

Clone this repo and `cd` into it, and run `./build.sh` to build the reference
interpreter `bin/robinri`, and the slightly-less-impractical interpreter
called `bin/whitecap` (for historical reasons, and subject to change.)

(Or if you have [toolshelf][], just run `toolshelf dock gh:catseye/robin`.)

If you have a few minutes to spare, please do run the tests by running
`./test.sh`.  (This requires [Falderal][].)

(There will be a link to a tutorial with further instructions in the future)

Documentation
-------------

Robin's fundamental semantics are documented in
[doc/Robin.markdown](doc/Robin.markdown).

History
-------

Robin 0.2 is a somewhat significant departure from Robin 0.1.  It keeps:

*   its syntax
*   its core builtins (mostly)
*   some of its standard modules ("small", list, environment, boolean, arith)
*   exceptions (and makes them standard rather than optional)
*   its zealous system agnosticism
*   its zealous disdain for escape characters (i.e. its literal string syntax)

Robin 0.2 *discards* from Robin 0.1:

*   bigrats.  Instead, in Robin 0.2 you get 32-bit signed integers (yes,
    precisely those.)  Anything else, you have to build.
*   its module system.  Robin has its own, much less hermetic/holistic
    system.  See the file `doc/Modules.markdown`.
*   concurrency.
*   I/O and side-effects.  It has reactors instead.  See `doc/Reactor.markdown`.
*   its grand ambitions.  Robin would rather exist than be perfect.

Robin 0.2 *adds* to Robin 0.1:

*   _reactors_, which I hope will be a cleaner and more system-agnostic
    way to do I/O.  See `doc/Reactor.markdown`.

[Falderal]:  http://catseye.tc/node/Falderal
[PicoLisp]:  http://picolisp.com/
[Pixley]:    http://catseye.tc/node/Pixley
[Robin]:     http://catseye.tc/node/Robin
[Scheme]:    http://schemers.org/
[toolshelf]: http://catseye.tc/node/toolshelf
