History of Robin
================

Robin 0.9
---------

*   Added a `cabalfile` for the purpose of tracking
    dependencies.

Robin 0.8
---------

*   What was previously known as a `macro` is now known
    as a `fexpr`.
*   The `fexpr` form no longer provides the `self`
    argument to its definition.  If recursion is desired
    in the definition of a fexpr, the fexpr should be
    written recursively (in the same way functions have
    traditionally been written recursively in Robin: pass
    the fexpr itself as the first argument to the fexpr.)
    The Robin definitions of fexprs in the standard library
    such as `let` and `list` have been rewritten this way.
*   The object that calling `fexpr` or `fun` produces is no
    longer called a "macro".  It is an "operator".  There
    there are other ways to obtain an operator than applying
    a `fexpr` or `fun` (for instance there have always been
    intrinsic operators; it's not fair to call them "macros".)
*   When a reactor produces an abort value, it does not cause
    a further event reporting the abort value to occur.

In the standard library,

*   The Robin definition of `bind` now checks that the name
    being bound is a symbol.  The Robin definition of `let`
    is now based on that of `bind` so it inherits this behaviour.
*   The documentation for the alist functions in the standard
    library was improved.
*   Added the `bind-vals` operator, which is like `bind-args`
    but does not evaluate the arguments, and which works on
    possibly-deep lists.

For the reference implementation,

*   When exceptions were replaced with abort values in 0.6, the
    evaluator wasn't fully adapted to handling abort values in
    all places.  Evaluator support has been changed to make it
    harder to forget to check for the abort value when needed.
*   Fixed recent import changes which prevented it from
    running under Hugs.
*   The `Macro` type of expressions has been removed,
    and `Builtin` renamed `Operator`.
*   No builtins are exposed by default.  The `--no-builtins`
    flag was replaced by the `--enable-builtins` flag, which
    has the complementary effect.

Robin 0.7
---------

*   The meaning of multiple `define`s of the same symbol
    has changed: it is allowed for the purposes of providing
    multiple semantically equivalent definitions of a symbol.
    The implementation is allowed to (but not required to) try
    to disprove the definitions are semantically equivalent.
    This obviates the need for `define-if-absent`, which has
    been removed.
*   Two macro values are now considered equal if their
    definitions are exactly equal (intensional equality
    sans alpha-conversion).
*   Abort values now have a defined representation.

For the reference implementation,

*   There are now QuickCheck tests that check whether
    the multiple definitions for a symbol, when given,
    are semantically equivalent.
*   `error` is no longer used anywhere in the source.

Robin 0.6
---------

*   Exceptions and exception handlers were removed from
    the language.  This is because conventional exception
    handlers have dynamic scope, and thus break referential
    transparency, while lexical exception handlers are of
    limited usefulness.
*   In their place are abort values.  The `raise` intrinsic
    is replaced by `abort`, which evaluates to
    an abort value.  An abort value is the wrong type for
    most operations, which results in another abort value
    (indicating a type error), so aborts cascade upwards.
*   The `catch` intrinsic is replaced by `recover`, which
    is intended for testing and handling abort values;
    it takes 5 arguments.
*   Lookup of values in an environment is more forgiving;
    if the alist for the environment is malformed, lookup
    will not complain about the malformedness of it; the
    identifier being sought will simply not be found.

For the reference implementation,

*   Clean up of source code: stylistic and refactoring.
*   No dedicated `Env` ADT; environments are `Expr`s.
*   No dedicated `IEnv` type - no more exception handler.

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
