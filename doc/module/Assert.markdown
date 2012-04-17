Module `assert`
===============

    -> Tests for functionality "Interpret Robin Program"

### `assert` ###

    | (robin (0 1) ((small (0 1) *) (assert (0 1) *))
    |   (assert (equal? 5 5) 7))
    = 7

    | (robin (0 1) ((small (0 1) *) (assert (0 1) *))
    |   (assert (equal? 5 6) 7))
    ? uncaught exception: (assertion-failed (equal? 5 6))

### `assert-boolean` ###

    | (robin (0 1) ((small (0 1) *) (assert (0 1) *))
    |   (assert-boolean #f 4))
    = 4

    | (robin (0 1) ((small (0 1) *) (assert (0 1) *))
    |   (assert-boolean 4 #f))
    ? uncaught exception: (expected-boolean 4)

### `assert-number` ###

    | (robin (0 1) ((small (0 1) *) (assert (0 1) *))
    |   (assert-number 4 #f))
    = #f

    | (robin (0 1) ((small (0 1) *) (assert (0 1) *))
    |   (assert-number #f 4))
    ? uncaught exception: (expected-number #f)

### `assert-symbol` ###

    | (robin (0 1) ((small (0 1) *) (assert (0 1) *))
    |   (assert-symbol (literal captain) (literal tennille)))
    = tennille

    | (robin (0 1) ((small (0 1) *) (assert (0 1) *))
    |   (assert-symbol 7 (literal what)))
    ? uncaught exception: (expected-symbol 7)

### `assert-list` ###

    | (robin (0 1) ((small (0 1) *) (assert (0 1) *))
    |   (assert-list () #t))
    = #t

    | (robin (0 1) ((small (0 1) *) (assert (0 1) *))
    |   (assert-list (pair 1 ()) #t))
    = #t

    | (robin (0 1) ((small (0 1) *) (assert (0 1) *))
    |   (assert-list #t (literal zunk)))
    ? uncaught exception: (expected-list #t)

### `assert-macro` ###

    | (robin (0 1) ((small (0 1) *) (assert (0 1) *))
    |   (assert-macro literal (literal it-is)))
    = it-is

    | (robin (0 1) ((small (0 1) *) (assert (0 1) *))
    |   (assert-macro (macro (self args env) args) (literal it-is)))
    = it-is

    | (robin (0 1) ((small (0 1) *) (assert (0 1) *))
    |   (assert-macro 7 (literal what)))
    ? uncaught exception: (expected-macro 7)
