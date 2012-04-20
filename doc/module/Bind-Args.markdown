Robin - Bind-Args module (provisional)
======================================

    -> Tests for functionality "Interpret Robin Program"

`bind-args` is a macro for binding the arguments of another value to
identifiers, as well as asserting that the correct number of arguments
have been given to the macro.

This macro should really be defined in `small`, and used by the other
macros defined in `small` (right now they don't complain if given too
many arguments, and complain about an `expected-list` if given too few.)

### `bind-args` ###

`bind-args` takes a literal list of identifiers, and expresion which
evaluates to a literal list of expressions whose values are to be bound
to those identifiers, an expresion which evaluates to the environment in
which those expressions will be evaluated, and an expression to evaluate
in the new environment in which the identifiers are bound.

    | (robin (0 1) ((small (0 1) *) (bind-args (0 1) *))
    |   (bind-args (a b) (literal (1 2)) (env)
    |     (list a b)))
    = (1 2)

Expressions in the list of values are evaluated.

    | (robin (0 1) ((small (0 1) *) (bind-args (0 1) *))
    |   (bind-args (a b) (literal ((subtract 5 4) (subtract 10 1))) (env)
    |     (list a b)))
    = (1 9)

Too many or too few arguments will raise an `illegal-arguments`
exception.

    | (robin (0 1) ((small (0 1) *) (bind-args (0 1) *))
    |   (bind-args (a b) (literal (1)) (env)
    |     (list a b)))
    ? uncaught exception: (illegal-arguments (1))

    | (robin (0 1) ((small (0 1) *) (bind-args (0 1) *))
    |   (bind-args (a b) (literal (1 2 3)) (env)
    |     (list a b)))
    ? uncaught exception: (illegal-arguments (1 2 3))

The literal arguments are reported in the exception.

    | (robin (0 1) ((small (0 1) *) (bind-args (0 1) *))
    |   (bind-args (a) (literal ((subtract 5 4) (subtract 1 0))) (env)
    |     a))
    ? uncaught exception: (illegal-arguments ((subtract 5 4) (subtract 1 0)))

This is how it might be used in a macro definition.  The reason for the
seemingly strange requirements of the second and third arguments should
become clear here: typically you would just pass the macro's `args` and
`env` to those arguments.

    | (robin (0 1) ((small (0 1) *) (bind-args (0 1) *))
    |   (bind add (macro (self args env)
    |     (bind-args (a b) args env
    |       (subtract a (subtract 0 b))))
    |     (add 4 (add 5 6))))
    = 15

    | (robin (0 1) ((small (0 1) *) (bind-args (0 1) *))
    |   (bind add (macro (self args env)
    |     (bind-args (a b) args env
    |       (subtract a (subtract 0 b))))
    |     (bind r 7
    |       (add r r))))
    = 14

    | (robin (0 1) ((small (0 1) *) (bind-args (0 1) *))
    |   (bind add (macro (self args env)
    |     (bind-args (a b) args env
    |       (subtract a (subtract 0 b))))
    |     (add (subtract 0 0))))
    ? uncaught exception: (illegal-arguments ((subtract 0 0)))
