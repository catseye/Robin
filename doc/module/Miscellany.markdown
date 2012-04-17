Miscellanous Module Tests
=========================

    -> Tests for functionality "Interpret Robin Program"

This document contains miscellaneous tests for module functionality;
tests that require multiple modules be loaded, and so forth.

Modules are cached, so that a module referenced by two other modules
is not loaded twice.

    | (robin (0 1) ((core (0 1) *) (random-a (0 1) *) (random-b (0 1) *))
    |   (equal? random-a random-b))
    = #t

Circular module imports produce an error rather than going into an infinite
loop.

    | (robin (0 1) ((core (0 1) *) (circular-a (0 1) *))
    |   (equal? literal-a literal-b))
    ? circular reference in module circular-a

`and` is short-circuiting.

    | (robin (0 1) ((small (0 1) *) (boolean (0 1) *) (concurrency (0 1) *) (crude-io (0 1) *))
    |   (let ((true
    |          (fun () (call! crude-output write (literal t) reply #t)))
    |         (false
    |          (fun () (call! crude-output write (literal f) reply #f))))
    |     (and (true) (false) (false) (true))))
    = t
    = f
    = #f

`or` is short-circuiting.

    | (robin (0 1) ((small (0 1) *) (boolean (0 1) *) (concurrency (0 1) *) (crude-io (0 1) *))
    |   (let ((true
    |          (fun () (call! crude-output write (literal t) reply #t)))
    |         (false
    |          (fun () (call! crude-output write (literal f) reply #f))))
    |     (or (false) (true) (true) (false))))
    = f
    = t
    = #t
