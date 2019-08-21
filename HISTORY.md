History of Robin
================

Robin 0.3 (ca 2019)
---------

Currently in progress.

*   The "intrinsics wrappers" were removed.  Their semantics have been
    incorporated into the intrinsics themselves (whose names no longer
    begin with `@`.)  The rationale is that they were hardly more complex
    than the intrinsics themselves.
*   TODO vague plan to reform the "reactor" subsystem.

For the reference implementation,

*   There is only one executable now, and it's called `robin`
*   The Haskell modules are in the namespace `Language.Robin`

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
*   its module system.  Robin has its own, much less hermetic/holistic
    system.  See the file [doc/Modules.md](doc/Modules.md).
*   concurrency.
*   I/O and side-effects.  It has reactors instead.
*   its grand ambitions.  Robin would rather exist than be perfect.

Robin 0.2 *adds* to Robin 0.1:

*   _reactors_, which I hope will be a cleaner and more system-agnostic
    way to do I/O.  See [doc/Reactor.md](doc/Reactor.md).

Robin 0.1 (ca 2012)
---------

Initial language.
