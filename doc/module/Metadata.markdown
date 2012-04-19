Module `metadata`
=================

    -> Tests for functionality "Interpret Robin Program"

Robin's `metadata` module exports...

### `with` ###

`with` attaches metadata to a value.

    | (robin (0 1) ((metadata (0 1)))
    |   (metadata:with #t 5))
    = 5

Passing values with metadata attached shouldn't break any of the core
macros.

    | (robin (0 1) ((core (0 1) *) (metadata (0 1) *))
    |   (head (with #t (prepend 1 ()))))
    = 1

    | (robin (0 1) ((core (0 1) *) (metadata (0 1) *))
    |   (tail (with #t (prepend 1 ()))))
    = ()

    | (robin (0 1) ((core (0 1) *) (metadata (0 1) *))
    |   (prepend (with #t 1) (prepend (with #t 2) ())))
    = (1 2)

    | (robin (0 1) ((core (0 1) *) (metadata (0 1) *))
    |   (if (with #t #t) (with #t 2) (with #t 3)))
    = 2

    | (robin (0 1) ((core (0 1) *) (metadata (0 1) *))
    |   (if (with 777 #f) (with #t 2) (with #t 3)))
    = 3

    | (robin (0 1) ((core (0 1) *) (metadata (0 1) *))
    |   (equal? (with #t 4) (with #t 4)))
    = #t

    | (robin (0 1) ((core (0 1) *) (metadata (0 1) *))
    |   (list? (with #t (prepend 2 (prepend 3 ())))))
    = #t

    | (robin (0 1) ((core (0 1) *) (metadata (0 1) *))
    |   (number? (with #t 3)))
    = #t

    | (robin (0 1) ((small (0 1) *) (metadata (0 1) *))
    |   (symbol? (with #t (literal x))))
    = #t

    | (robin (0 1) ((core (0 1) *) (metadata (0 1) *))
    |   (macro? (with #t (macro (self args env) args))))
    = #t

    | (robin (0 1) ((core (0 1) *) (metadata (0 1) *))
    |   (boolean? (with #t #t)))
    = #t

    | (robin (0 1) ((core (0 1) *) (metadata (0 1) *))
    |   ((with #t (macro (self args env) args)) foo bar baz))
    = (foo bar baz)

    | (robin (0 1) ((core (0 1) *) (metadata (0 1) *))
    |   (subtract (with #t 0) (with #f 5)))
    = -5

    | (robin (0 1) ((core (0 1) *) (metadata (0 1) *))
    |   (divide (with #t 1) (with #f 5)))
    = 1/5

    | (robin (0 1) ((core (0 1) *) (metadata (0 1) *))
    |   (sign (with #t 1)))
    = 1

    | (robin (0 1) ((core (0 1) *) (metadata (0 1) *))
    |   (floor (with #t 3/2)))
    = 1

Testing for equality ignores metadata.

    | (robin (0 1) ((core (0 1) *) (metadata (0 1) *))
    |   (equal? (with #t 4) 4))
    = #t

    | (robin (0 1) ((core (0 1) *) (metadata (0 1) *))
    |   (equal? 4 (with #t 4)))
    = #t

    | (robin (0 1) ((core (0 1) *) (metadata (0 1) *))
    |   (equal? (with #t 4) (with #f 4)))
    = #t

    | (robin (0 1) ((core (0 1) *) (metadata (0 1) *))
    |   (equal? (with #t 4) (with #t 5)))
    = #f

### `has?` ###

`has?` checks if a value has a given metadata.

    | (robin (0 1) ((metadata (0 1)))
    |   (metadata:has? #t 4))
    = #f

    | (robin (0 1) ((metadata (0 1)))
    |   (metadata:has? #t (metadata:with #t 4)))
    = #t

    | (robin (0 1) ((core (0 1) *) (metadata (0 1) *))
    |   (has? #t (head (prepend (with #t 4) (prepend 5 ())))))
    = #t

Binding retains metadata.

    | (robin (0 1) ((small (0 1) *) (metadata (0 1) *))
    |   (bind q (with (literal gee) 77)
    |     (has? (literal gee) q)))
    = #t

Metadata is retained in macros.

    | (robin (0 1) ((small (0 1) *) (metadata (0 1) *))
    |   (bind whee
    |     (macro (self args env)
    |       (prepend (head env)
    |             (prepend (has? 7 (head (tail (head env)))) ())))
    |     (bind r (with 7 8)
    |       (whee))))
    = ((r 8) #t)
