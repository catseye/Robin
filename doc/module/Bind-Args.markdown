
    -> Tests for functionality "Interpret Robin Program"

### `bind-args` ###

    | (robin (0 1) ((small (0 1) *) (bind-args (0 1) *))
    |   (bind-args (a b) (list 1 2)
    |     (list a b)))
    = (1 2)

    | (robin (0 1) ((small (0 1) *) (bind-args (0 1) *))
    |   (bind-args (a b) (list 1)
    |     (list a b)))
    ? uncaught exception: (illegal-arguments (1))

    | (robin (0 1) ((small (0 1) *) (bind-args (0 1) *))
    |   (bind-args (a b) (list 1 2 3)
    |     (list a b)))
    ? uncaught exception: (illegal-arguments (1 2 3))
