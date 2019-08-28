History of Robin
================

Robin 0.4 (autumn 2019)
---------

*   Improved modularity of the specification.
*   Many tests are for the Robin Expression Language, and have been
    made explicitly so, instead of for the Robin Toplevel Language.
*   Added `bound?` predicate to env lib in stdlib.
*   The `require` top-level form was added.

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
