Module `arith`
==============

    -> Tests for functionality "Interpret Robin Program"

### `+` ###

    | (robin (0 1) ((arith (0 1) *))
    |   (+ 14 23))
    = 37

### `-` ###

    | (robin (0 1) ((arith (0 1) *))
    |   (- 23 10))
    = 13

### `*` ###

    | (robin (0 1) ((arith (0 1) *))
    |   (* 23 10))
    = 230

### `/` ###

    | (robin (0 1) ((arith (0 1) *))
    |   (/ 33 11))
    = 3

### `abs` ###

    | (robin (0 1) ((arith (0 1) *))
    |   (abs 5))
    = 5

    | (robin (0 1) ((arith (0 1) *))
    |   (abs (- 0 5)))
    = 5

    | (robin (0 1) ((arith (0 1) *))
    |   (abs 0))
    = 0

### `frac` ###

    | (robin (0 1) ((arith (0 1) *))
    |   (frac 6/5))
    = 1/5

    | (robin (0 1) ((arith (0 1) *))
    |   (frac (- 0 6/5)))
    = 1/5

    | (robin (0 1) ((arith (0 1) *))
    |   (frac 8))
    = 0

### `integer?` ###

    | (robin (0 1) ((arith (0 1) *))
    |   (integer? 6/5))
    = #f

    | (robin (0 1) ((arith (0 1) *))
    |   (integer? 8))
    = #t

    | (robin (0 1) ((arith (0 1) *))
    |   (integer? 0))
    = #t

    | (robin (0 1) ((arith (0 1) *))
    |   (integer? (- 0 8)))
    = #t

### `div` ###

    | (robin (0 1) ((arith (0 1) *))
    |   (div 100 3))
    = 33

    | (robin (0 1) ((arith (0 1) *))
    |   (div (- 0 100) 3))
    = -34

    | (robin (0 1) ((arith (0 1) *))
    |   (div 100 (- 0 3)))
    = -34

    | (robin (0 1) ((arith (0 1) *))
    |   (div 1001/10 3))
    = 33

    | (robin (0 1) ((arith (0 1) *))
    |   (div 100 10/3))
    = 33

    | (robin (0 1) ((arith (0 1) *))
    |   (div 10 0))
    ? uncaught exception: (division-by-zero 10)

### `rem` ###

    | (robin (0 1) ((arith (0 1) *))
    |   (rem 12 3))
    = 0

    | (robin (0 1) ((arith (0 1) *))
    |   (rem 11 3))
    = 2

    | (robin (0 1) ((arith (0 1) *))
    |   (rem 10 3))
    = 1

    | (robin (0 1) ((arith (0 1) *))
    |   (rem 9 3))
    = 0

    | (robin (0 1) ((arith (0 1) *))
    |   (rem (- 0 10) 3))
    = 2

    | (robin (0 1) ((arith (0 1) *))
    |   (rem 10 (- 0 3)))
    = -2

    | (robin (0 1) ((arith (0 1) *))
    |   (rem 10 0))
    ? uncaught exception: (division-by-zero 10)

### `>` ###

    | (robin (0 1) ((arith (0 1) *))
    |   (> 6 4))
    = #t

    | (robin (0 1) ((arith (0 1) *))
    |   (> 6 8))
    = #f

    | (robin (0 1) ((arith (0 1) *))
    |   (> 6 6))
    = #f

### `<` ###

    | (robin (0 1) ((arith (0 1) *))
    |   (< 6 4))
    = #f

    | (robin (0 1) ((arith (0 1) *))
    |   (< 6 8))
    = #t

    | (robin (0 1) ((arith (0 1) *))
    |   (< 6 6))
    = #f

### `>=` ###

    | (robin (0 1) ((arith (0 1) *))
    |   (>= 6 4))
    = #t

    | (robin (0 1) ((arith (0 1) *))
    |   (>= 6 8))
    = #f

    | (robin (0 1) ((arith (0 1) *))
    |   (>= 6 6))
    = #t

### `<=` ###

    | (robin (0 1) ((arith (0 1) *))
    |   (<= 6 4))
    = #f

    | (robin (0 1) ((arith (0 1) *))
    |   (<= 6 8))
    = #t

    | (robin (0 1) ((arith (0 1) *))
    |   (<= 6 6))
    = #t

### `natural?` ###

    | (robin (0 1) ((arith (0 1) *))
    |   (natural? 6/5))
    = #f

    | (robin (0 1) ((arith (0 1) *))
    |   (natural? 8))
    = #t

    | (robin (0 1) ((arith (0 1) *))
    |   (natural? 0))
    = #t

    | (robin (0 1) ((arith (0 1) *))
    |   (natural? (- 0 8)))
    = #f
