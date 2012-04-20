
    -> Tests for functionality "Interpret Robin Program"

### `bind-args` ###

    | (robin (0 1) ((small (0 1) *) (bind-args (0 1) *))
    |   (bind-args (a b) (1 2)
    |     (list a b)))
    = (1 2)

Expressions in the list of values are evaluated.

    | (robin (0 1) ((small (0 1) *) (bind-args (0 1) *))
    |   (bind-args (a b) ((subtract 5 4) (subtract 10 1))
    |     (list a b)))
    = (1 9)

Too many or too few arguments will raise an `illegal-arguments`
exception.

    | (robin (0 1) ((small (0 1) *) (bind-args (0 1) *))
    |   (bind-args (a b) (1)
    |     (list a b)))
    ? uncaught exception: (illegal-arguments (1))

    | (robin (0 1) ((small (0 1) *) (bind-args (0 1) *))
    |   (bind-args (a b) (1 2 3)
    |     (list a b)))
    ? uncaught exception: (illegal-arguments (1 2 3))

The literal arguments are reported in the exception.

    | (robin (0 1) ((small (0 1) *) (bind-args (0 1) *))
    |   (bind-args (a) ((subtract 5 4) (subtract 1 0))
    |     a))
    ? uncaught exception: (illegal-arguments ((subtract 5 4) (subtract 1 0)))
