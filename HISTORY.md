History of Robin
================

Robin 0.6
---------

*   Exceptions and exception handlers were removed from
    the language.  This is because conventional exception
    handlers have dynamic scope, and thus break referential
    transparency, while lexical exception handlers are of
    limited usefulness.
    
    In their place are abort values.  `abort` evaluates to
    an abort value.  An abort value is the wrong type for
    most operations, which results in another abort value
    (indicating a type error), so aborts cascade upwards.
    The `catch` intrinsic is now intended for testing and
    handling abort values; it now takes 5 arguments.
*   Lookup of values in an environment is more forgiving;
    if the alist for the environment is malformed, lookup
    will not complain about the malformedness of it; the
    identifier being sought will simply not be found.

For the reference implementation,

*   Clean up of source code: stylistic and refactoring.
*   No dedicated `Env` ADT; environments are `Expr`s.
*   No dedicated `IEnv` type; exception handler is in `Env`.

Robin 0.5 (Late Sep 2019)
---------

*   The `define-if-absent` top-level form was added.
*   Fixed a bug in definition of `multiply`.
*   Renamed `>`, `>=`, `<`, and `<=` to `gt?`, `gte?`,
    `lt?`, and `lte?` respectively.
*   Several definitions fixed to not expose extra symbols
    that they don't define.

For the reference implementation,

*   Implemented built-in versions of symbols in `arith` package.
*   Refactored the reactors module into Reactors, EventLoop, and
    Facilities.  The EventLoop does not rely on any particular
    facilities; they are (dependency-)injected by the Main module.
*   Allowed the reference implementation to run under Hugs,
    in part by providing a `HugsMain.hs` which only includes the
    facilities that are supported by Hugs.
*   Modelled environments with their own algebraic data type
    (`Env`).  Experimented with implementing them with
    `Data.Map.Strict`, with limited success (Robin's semantics
    define an environment to be able to shadow old bindings.)

Robin 0.4 (Early Sep 2019)
---------

*   Reworked entire specification document, making it properly modular.
*   Many tests are for the Robin Expression Language, and have been
    made explicitly so, instead of for the Robin Toplevel Language.
*   The `bound?` predicate was added to env lib in stdlib.
*   The `require` top-level form was added.
*   The `write` command was added to the definition of `line-terminal`.
*   The `random-u16-source` facility was added to the Reactors
    specification and to the reference implementation.
*   Fixed shortcomings in `<` and `>` where operating on large numbers
    would give incorrect results (thanks wob_jonas!)
*   Clarified what Robin borrows from PicoLisp (thanks arseniiv!)

For the reference implementation,

*   Added `eval` command-line option, to evaluate a Robin expression
    given in a text file (mostly to support Robin Expression tests.)
*   Added a small, crude QuickCheck test suite.

Robin 0.3 (Aug 2019)
---------

*   The "intrinsics wrappers" were removed.  Their semantics have been
    incorporated into the intrinsics themselves (whose names no longer
    begin with `@`.)  The rationale is that they were hardly more complex
    than the intrinsics themselves.
*   The "fun" package has also been merged into "small", since without
    intrinsics wrappers there is little reason to keep it separate.
*   The reactor subsystem was reformed.  Reactors define a transducer
    function that takes an event and a state, and returns a new state
    and a list of commands, which are simply new events.  Several other
    details were cleared up, and the implementation was re-written.
*   The `assert` top-level form was added.

For the reference implementation,

*   There is only one executable now, and it's called `robin`
*   The Haskell modules are in the namespace `Language.Robin`
*   A multitude of other small cleanups.

Robin 0.2 (ca 2014)
---------

Robin 0.2 was a somewhat significant departure from Robin 0.1.  It kept:

*   its syntax
*   its core builtins (mostly)
*   some of its standard modules ("small", list, environment, boolean, arith)
*   exceptions (and makes them standard rather than optional)
*   its zealous system agnosticism
*   its zealous disdain for escape characters (i.e. its literal string syntax)

Robin 0.2 *discards* from Robin 0.1:

*   bigrats.  Instead, in Robin 0.2 you get 32-bit signed integers (yes,
    precisely those.)  Anything else, you have to build.
*   its module system.  Robin 0.2 has its own, much less hermetic/holistic
    system.
*   concurrency.
*   I/O and side-effects.  It has reactors instead.
*   its grand ambitions.  Robin would rather exist than be perfect.

Robin 0.2 *adds* to Robin 0.1:

*   _reactors_, which I hope will be a cleaner and more system-agnostic
    way to do I/O.

Robin 0.1 (ca 2012)
---------

Initial language.

Pre-history (ca 2010)
-----------

Robin was originally a design for a [Pixley][]-based operating system (or something
similar to an operating system) which was heavily resource-oriented; almost
everything, including every concurrent process, was a virtual device
which must be acquired from a central resource arbiter.  This arbiter could
satisfy the constraints specified when requesting a device any way it saw
fit; so the operating environment potentially had a lot of influence over
exactly what any given program does.

Not a lot of that idea remains, but it did influence the fact that Robin should
be a purely functional language which nevertheless interacts with the rest of the
world through some kind of framework.

[Pixley]:    https://catseye.tc/node/Pixley
