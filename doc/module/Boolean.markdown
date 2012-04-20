Module `boolean`
================

    -> Tests for functionality "Interpret Robin Program"

### `and` ###

`and` evaluates all of its arguments to booleans, and evaluates to the
logical conjunction (boolean "and") of all of these values.

    | (robin (0 1) ((boolean (0 1) *))
    |   (and #t #t))
    = #t

    | (robin (0 1) ((boolean (0 1) *))
    |   (and #t #f))
    = #f

    | (robin (0 1) ((boolean (0 1) *))
    |   (and #f #t))
    = #f

    | (robin (0 1) ((boolean (0 1) *))
    |   (and #f #f))
    = #f

`and` can take any number of arguments.

    | (robin (0 1) ((boolean (0 1) *))
    |   (and))
    = #t

    | (robin (0 1) ((boolean (0 1) *))
    |   (and #f))
    = #f

    | (robin (0 1) ((boolean (0 1) *))
    |   (and #t #t #t #t #t))
    = #t

    | (robin (0 1) ((boolean (0 1) *))
    |   (and #t #t #t #f))
    = #f

`and` expects its arguments to be booleans.

    | (robin (0 1) ((boolean (0 1)))
    |   (boolean:and 100))
    ? uncaught exception: (expected-boolean 100)

`and` is short-circuiting in the sense that no arguments after the first
`#f` argument will be evaluated.  Fully testing this requires side-effects,
but it can be demonstrated as follows.

    | (robin (0 1) ((boolean (0 1)))
    |   (boolean:and #f 100))
    = #f

### `conj` ###

`conj` evaluates its single argument to a list of booleans, then evaluates
to the logical conjunction of those booleans.

    | (robin (0 1) ((boolean (0 1) *))
    |   (conj ()))
    = #t

    | (robin (0 1) ((boolean (0 1) *) (small (0 1) *))
    |   (conj (list #t #t #t)))
    = #t

    | (robin (0 1) ((boolean (0 1) *) (small (0 1) *))
    |   (conj (list #t #t #f)))
    = #f

`conj` expects exactly one argument.

    | (robin (0 1) ((boolean (0 1) *) (small (0 1) *))
    |   (conj))
    ? uncaught exception: (illegal-arguments ())

    | (robin (0 1) ((boolean (0 1) *) (small (0 1) *))
    |   (conj (list #t #t) (list #f #f)))
    ? uncaught exception: (illegal-arguments ((list #t #t) (list #f #f)))

`conj` expects its single argument to be a list.

    | (robin (0 1) ((boolean (0 1) *) (small (0 1) *))
    |   (conj 100))
    ? uncaught exception: (expected-list 100)

`conj` expects its single argument to be a list of booleans.

    | (robin (0 1) ((boolean (0 1) *) (small (0 1) *))
    |   (conj (list #t #t 100)))
    ? uncaught exception: (expected-boolean 100)

`conj` is short-circuiting in the sense that no elements after the first
`#f` in the list will be examined.

    | (robin (0 1) ((boolean (0 1) *) (small (0 1) *))
    |   (conj (list #f #t 100)))
    = #f

### `or` ###

`or` evaluates all of its arguments to booleans, and evaluates to the
logical disjunction (boolean "or") of all of these values.

    | (robin (0 1) ((boolean (0 1) *))
    |   (or #t #t))
    = #t

    | (robin (0 1) ((boolean (0 1) *))
    |   (or #t #f))
    = #t

    | (robin (0 1) ((boolean (0 1) *))
    |   (or #f #t))
    = #t

    | (robin (0 1) ((boolean (0 1) *))
    |   (or #f #f))
    = #f

`or` can take any number of arguments.

    | (robin (0 1) ((boolean (0 1) *))
    |   (or))
    = #f

    | (robin (0 1) ((boolean (0 1) *))
    |   (or #t))
    = #t

    | (robin (0 1) ((boolean (0 1) *))
    |   (or #f #f #f #f #f))
    = #f

    | (robin (0 1) ((boolean (0 1) *))
    |   (or #f #f #f #t))
    = #t

`or` expects its arguments to be booleans.

    | (robin (0 1) ((boolean (0 1)))
    |   (boolean:or 100))
    ? uncaught exception: (expected-boolean 100)

`or` is short-circuiting in the sense that no arguments after the first
`#t` argument will be evaluated.  Fully testing this requires side-effects,
but it can be demonstrated as follows.

    | (robin (0 1) ((boolean (0 1)))
    |   (boolean:or #t 100))
    = #t

### `disj` ###

`disj` evaluates its single argument to a list of booleans, then evaluates
to the logical disjunction of those booleans.

    | (robin (0 1) ((boolean (0 1) *))
    |   (disj ()))
    = #f

    | (robin (0 1) ((boolean (0 1) *) (small (0 1) *))
    |   (disj (list #f #f #f)))
    = #f

    | (robin (0 1) ((boolean (0 1) *) (small (0 1) *))
    |   (disj (list #f #t #f)))
    = #t

`disj` expects exactly one argument.

    | (robin (0 1) ((boolean (0 1) *) (small (0 1) *))
    |   (disj))
    ? uncaught exception: (illegal-arguments ())

    | (robin (0 1) ((boolean (0 1) *) (small (0 1) *))
    |   (disj (list #t #t) (list #f #f)))
    ? uncaught exception: (illegal-arguments ((list #t #t) (list #f #f)))

`disj` expects its single argument to be a list.

    | (robin (0 1) ((boolean (0 1) *) (small (0 1) *))
    |   (disj 100))
    ? uncaught exception: (expected-list 100)

`disj` expects its single argument to be a list of booleans.

    | (robin (0 1) ((boolean (0 1) *) (small (0 1) *))
    |   (disj (list #f #f 100)))
    ? uncaught exception: (expected-boolean 100)

`disj` is short-circuiting in the sense that no elements after the first
`#t` in the list will be examined.

    | (robin (0 1) ((boolean (0 1) *) (small (0 1) *))
    |   (disj (list #f #t 100)))
    = #t

### `not` ###

`not` evaluates its single argument to a boolean, then evaluates to
the logical negation of that boolean.

    | (robin (0 1) ((boolean (0 1) *))
    |   (not #t))
    = #f

    | (robin (0 1) ((boolean (0 1) *))
    |   (not #f))
    = #t

`not` expects exactly one argument.

    | (robin (0 1) ((boolean (0 1) *))
    |   (not))
    ? uncaught exception: (illegal-arguments ())

    | (robin (0 1) ((boolean (0 1) *))
    |   (not #t #f))
    ? uncaught exception: (illegal-arguments (#t #f))

`not` expects its single argument to be a boolean.

    | (robin (0 1) ((boolean (0 1)))
    |   (boolean:not 33))
    ? uncaught exception: (expected-boolean 33)

### `xor` ###

`xor` evaluates both of its arguments to boolean, then evaluates to
the "exclusive-or" of those booleans.

    | (robin (0 1) ((boolean (0 1) *))
    |   (xor #t #t))
    = #f

    | (robin (0 1) ((boolean (0 1) *))
    |   (xor #t #f))
    = #t

    | (robin (0 1) ((boolean (0 1) *))
    |   (xor #f #t))
    = #t

    | (robin (0 1) ((boolean (0 1) *))
    |   (xor #f #f))
    = #f

`xor` expects exactly two arguments.

    | (robin (0 1) ((boolean (0 1) *))
    |   (xor #f))
    ? uncaught exception: (illegal-arguments (#f))

    | (robin (0 1) ((boolean (0 1) *))
    |   (xor #t #f #f))
    ? uncaught exception: (illegal-arguments (#t #f #f))

`xor` expects both of its arguments to be booleans.

    | (robin (0 1) ((boolean (0 1)))
    |   (boolean:xor 100 #t))
    ? uncaught exception: (expected-boolean 100)

    | (robin (0 1) ((boolean (0 1)))
    |   (boolean:xor #t 99))
    ? uncaught exception: (expected-boolean 99)

This test demonstrates that these functions really do evaluate their
arguments.

    | (robin (0 1) ((boolean (0 1) *))
    |   (and (or (xor (and #t (not (not #t))) #f) #f) #t))
    = #t
