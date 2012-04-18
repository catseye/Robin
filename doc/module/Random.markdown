Module `random`
===============

These tests constitute sanity checks; they do not actually test that
the generated numbers are random.

### `random` ###

    -> Tests for functionality "Interpret Robin Program"

    | (robin (0 1) ((small (0 1) *) (concurrency (0 1) *) (arith (0 1) *) (random (0 1) *))
    |   (call! random range (list 1 6) value
    |     (< value 7)))
    = #t

    | (robin (0 1) ((small (0 1) *) (concurrency (0 1) *) (arith (0 1) *) (random (0 1) *))
    |   (call! random range (list 1 6) value
    |     (>= value 1)))
    = #t
