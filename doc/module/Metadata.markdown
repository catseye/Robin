Module `metadata`
=================

    -> Tests for functionality "Interpret Robin Program"

Robin's `metadata` module exports macros for working with metadata.  Like
`exception`, importing this module both asserts that the Robin implementation
supports metadata on values, and exposes the macros used to work with
metadata (`with` and `has?`.)

### `with` ###

`with` attaches metadata to a value.

    | (robin (0 1) ((metadata (0 1)))
    |   (metadata:with cromulent #t 5))
    = 5

The name of the metadata is generally a symbol; it is not evaluated.

    | (robin (0 1) ((metadata (0 1)))
    |   (metadata:with (vorp dorp) #t 5))
    = 5

Passing values with metadata attached shouldn't break any of the core
macros.

    | (robin (0 1) ((core (0 1) *) (metadata (0 1) *))
    |   (head (with spiffy #t (prepend 1 ()))))
    = 1

    | (robin (0 1) ((core (0 1) *) (metadata (0 1) *))
    |   (tail (with snazzy #t (prepend 1 ()))))
    = ()

    | (robin (0 1) ((core (0 1) *) (metadata (0 1) *))
    |   (prepend (with sassy #t 1) (prepend (with snotty #t 2) ())))
    = (1 2)

    | (robin (0 1) ((core (0 1) *) (metadata (0 1) *))
    |   (if (with sassy #t #t) (with snotty #t 2) (with insolent #t 3)))
    = 2

    | (robin (0 1) ((core (0 1) *) (metadata (0 1) *))
    |   (if (with weight 777 #f) (with height 888 2) (with width 999 3)))
    = 3

    | (robin (0 1) ((core (0 1) *) (metadata (0 1) *))
    |   (equal? (with fussy #t 4) (with picky #t 4)))
    = #t

    | (robin (0 1) ((core (0 1) *) (metadata (0 1) *))
    |   (list? (with pokey #t (prepend 2 (prepend 3 ())))))
    = #t

    | (robin (0 1) ((core (0 1) *) (metadata (0 1) *))
    |   (number? (with hirsute #t 3)))
    = #t

    | (robin (0 1) ((small (0 1) *) (metadata (0 1) *))
    |   (symbol? (with forgetful #t (literal x))))
    = #t

    | (robin (0 1) ((core (0 1) *) (metadata (0 1) *))
    |   (macro? (with special #t (macro (self args env) args))))
    = #t

    | (robin (0 1) ((core (0 1) *) (metadata (0 1) *))
    |   (boolean? (with squirrelly #t #t)))
    = #t

    | (robin (0 1) ((core (0 1) *) (metadata (0 1) *))
    |   ((with moosey #t (macro (self args env) args)) foo bar baz))
    = (foo bar baz)

    | (robin (0 1) ((core (0 1) *) (metadata (0 1) *))
    |   (subtract (with boris #t 0) (with natasha #f 5)))
    = -5

    | (robin (0 1) ((core (0 1) *) (metadata (0 1) *))
    |   (divide (with near #t 1) (with far #f 5)))
    = 1/5

    | (robin (0 1) ((core (0 1) *) (metadata (0 1) *))
    |   (sign (with graceful #t 1)))
    = 1

    | (robin (0 1) ((core (0 1) *) (metadata (0 1) *))
    |   (floor (with nervous #t 3/2)))
    = 1

Testing for equality ignores metadata.

    | (robin (0 1) ((core (0 1) *) (metadata (0 1) *))
    |   (equal? (with cromulent #t 4) 4))
    = #t

    | (robin (0 1) ((core (0 1) *) (metadata (0 1) *))
    |   (equal? 4 (with cromulent #t 4)))
    = #t

    | (robin (0 1) ((core (0 1) *) (metadata (0 1) *))
    |   (equal? (with cromulent #t 4) (with goofy #t 4)))
    = #t

    | (robin (0 1) ((core (0 1) *) (metadata (0 1) *))
    |   (equal? (with cromulent #t 4) (with cromulent #t 5)))
    = #f

Different metadata really shouldn't be put on objects which are `equal?`,
but you can do it.

    | (robin (0 1) ((core (0 1) *) (metadata (0 1) *))
    |   (equal? (with cromulent #t 4) (with cromulent #f 4)))
    = #t

### `has?` ###

`has?` checks if a value has a given metadata.

    | (robin (0 1) ((metadata (0 1)))
    |   (metadata:has? #t 4))
    = #f

    | (robin (0 1) ((metadata (0 1)))
    |   (metadata:has? cromulent (metadata:with cromulent #t 4)))
    = #t

    | (robin (0 1) ((metadata (0 1)))
    |   (metadata:has? cromulent (metadata:with cromulent #f 4)))
    = #t

    | (robin (0 1) ((core (0 1) *) (metadata (0 1) *))
    |   (has? cromulent (head (prepend (with cromulent #t 4) (prepend 5 ())))))
    = #t

Binding retains metadata.

    | (robin (0 1) ((small (0 1) *) (metadata (0 1) *))
    |   (bind q (with gee #t 77)
    |     (has? gee q)))
    = #t

Metadata is retained in macros.

    | (robin (0 1) ((small (0 1) *) (metadata (0 1) *))
    |   (bind whee
    |     (macro (self args env)
    |       (prepend (head env)
    |             (prepend (has? gee (head (tail (head env)))) ())))
    |     (bind r (with gee 7 8)
    |       (whee))))
    = ((r 8) #t)
