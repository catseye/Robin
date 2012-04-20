Module `arith`
==============

    -> Tests for functionality "Interpret Robin Program"

### `add` ###

`add` evaluates both of its arguments to numbers and evaluates to the sum
of those two numbers.

    | (robin (0 1) ((arith (0 1)))
    |   (arith:add 14 23))
    = 37

`add` expects exactly two arguments.

    | (robin (0 1) ((arith (0 1) *))
    |   (add 14))
    ? uncaught exception: (illegal-arguments (14))

    | (robin (0 1) ((arith (0 1) *))
    |   (add 6 7 7))
    ? uncaught exception: (illegal-arguments (6 7 7))

Both of the arguments to `add` must be numbers.

    | (robin (0 1) ((arith (0 1) *))
    |   (add 14 #t))
    ? uncaught exception: (expected-number #t)

    | (robin (0 1) ((arith (0 1) *))
    |   (add #t 51))
    ? uncaught exception: (expected-number #t)

### `+` ###

`+` evaluates all of its arguments to numbers and evaluates to the sum
of those numbers.

    | (robin (0 1) ((arith (0 1) *))
    |   (+ 14 23))
    = 37

`+` can accept any number of arguments.

    | (robin (0 1) ((arith (0 1) *))
    |   (+ 14 23 53))
    = 90

`+` with no arguments evaluates to zero.

    | (robin (0 1) ((arith (0 1) *))
    |   (+))
    = 0

`+` with one argument is an identity function on numbers.

    | (robin (0 1) ((arith (0 1) *))
    |   (+ 7))
    = 7

All of the arguments to `+` must be numbers.

    | (robin (0 1) ((arith (0 1) *))
    |   (+ 14 #t))
    ? uncaught exception: (expected-number #t)

    | (robin (0 1) ((arith (0 1) *))
    |   (+ #t 51))
    ? uncaught exception: (expected-number #t)

### `sum` ###

`sum` evaluates its single argument to a list of numbers.  It then evaluates
to the sum of all of the numbers in the list.

    | (robin (0 1) ((arith (0 1)) (small (0 1)))
    |   (arith:sum (small:list 77 35 128 4)))
    = 244

`sum` of an empty list is zero.

    | (robin (0 1) ((arith (0 1)))
    |   (arith:sum ()))
    = 0

`sum` expects exactly one argument.

    | (robin (0 1) ((arith (0 1) *) (small (0 1) *))
    |   (sum (list 4 5) (list 6 7)))
    ? uncaught exception: (illegal-arguments ((list 4 5) (list 6 7)))

    | (robin (0 1) ((arith (0 1) *))
    |   (sum))
    ? uncaught exception: (illegal-arguments ())

`sum` expects its one argument to be a list.

    | (robin (0 1) ((arith (0 1) *))
    |   (sum 41))
    ? uncaught exception: (expected-list 41)

`sum` expects its list to contain only numbers.

    | (robin (0 1) ((arith (0 1) *) (small (0 1)))
    |   (sum (small:list 4 5 6 #t 7 8)))
    ? uncaught exception: (expected-number #t)

### `-` ###

`-` evaluates both of its arguments to numbers and evaluates to the difference
of the second number from the first.

    | (robin (0 1) ((arith (0 1) *))
    |   (- 23 10))
    = 13

`-` expects exactly two arguments, both numbers.

    | (robin (0 1) ((arith (0 1) *))
    |   (- 14))
    ? uncaught exception: (illegal-arguments (14))

    | (robin (0 1) ((arith (0 1) *))
    |   (- 14 23 57))
    ? uncaught exception: (illegal-arguments (14 23 57))

    | (robin (0 1) ((arith (0 1) *))
    |   (- 14 #t))
    ? uncaught exception: (expected-number #t)

    | (robin (0 1) ((arith (0 1) *))
    |   (- #t 51))
    ? uncaught exception: (expected-number #t)

### `multiply` ###

`multiply` evaluates both of its arguments to numbers and evaluates to the product
of those two numbers.

    | (robin (0 1) ((arith (0 1)))
    |   (arith:multiply 6 7))
    = 42

`multiply` expects exactly two arguments.

    | (robin (0 1) ((arith (0 1) *))
    |   (multiply 14))
    ? uncaught exception: (illegal-arguments (14))

    | (robin (0 1) ((arith (0 1) *))
    |   (multiply 6 7 7))
    ? uncaught exception: (illegal-arguments (6 7 7))

Both of the arguments to `multiply` must be numbers.

    | (robin (0 1) ((arith (0 1) *))
    |   (multiply 14 #t))
    ? uncaught exception: (expected-number #t)

    | (robin (0 1) ((arith (0 1) *))
    |   (multiply #t 51))
    ? uncaught exception: (expected-number #t)

### `*` ###

`*` evaluates all of its arguments to numbers and evaluates to the product
of those numbers.

    | (robin (0 1) ((arith (0 1) *))
    |   (* 23 10))
    = 230

`*` can accept any number of arguments.

    | (robin (0 1) ((arith (0 1) *))
    |   (* 3 4 5))
    = 60

`*` with no arguments evaluates to one.

    | (robin (0 1) ((arith (0 1) *))
    |   (*))
    = 1

`*` with one argument is an identity function on numbers.

    | (robin (0 1) ((arith (0 1) *))
    |   (* 7/8))
    = 7/8

`*` expects all of its arguments to be numbers.

    | (robin (0 1) ((arith (0 1) *))
    |   (* 14 #t))
    ? uncaught exception: (expected-number #t)

    | (robin (0 1) ((arith (0 1) *))
    |   (* #t 51))
    ? uncaught exception: (expected-number #t)

### `product` ###

`product` evaluates its single argument to a list of numbers.  It then evaluates
to the product of all of the numbers in the list.

    | (robin (0 1) ((arith (0 1)) (small (0 1)))
    |   (arith:product (small:list 5 7 9 3)))
    = 945

`product` of an empty list is one.

    | (robin (0 1) ((arith (0 1)))
    |   (arith:product ()))
    = 1

`product` expects exactly one argument.

    | (robin (0 1) ((arith (0 1) *) (small (0 1) *))
    |   (product (list 4 5) (list 6 7)))
    ? uncaught exception: (illegal-arguments ((list 4 5) (list 6 7)))

    | (robin (0 1) ((arith (0 1) *))
    |   (product))
    ? uncaught exception: (illegal-arguments ())

`product` expects its one argument to be a list.

    | (robin (0 1) ((arith (0 1) *))
    |   (product 7))
    ? uncaught exception: (expected-list 7)

`product` expects its list to contain only numbers.

    | (robin (0 1) ((arith (0 1) *) (small (0 1)))
    |   (product (small:list 4 5 6 #t 7 8)))
    ? uncaught exception: (expected-number #t)

### `/` ###

`/` evaluates both of its arguments to numbers and evaluates to the
first number divided by the second.

    | (robin (0 1) ((arith (0 1) *))
    |   (/ 33 11))
    = 3

`/` works on any rational numbers, not just integers.

    | (robin (0 1) ((arith (0 1) *))
    |   (/ 33 4))
    = 33/4

    | (robin (0 1) ((arith (0 1) *))
    |   (/ (- 0 33) 4))
    = -33/4

Division by zero is undefined, and an exception will be raised.

    | (robin (0 1) ((arith (0 1) *))
    |   (/ 10 0))
    ? uncaught exception: (division-by-zero 10)

`/` expects exactly two arguments, both numbers.

    | (robin (0 1) ((arith (0 1) *))
    |   (/ 14))
    ? uncaught exception: (illegal-arguments (14))

    | (robin (0 1) ((arith (0 1) *))
    |   (/ 14 23 57))
    ? uncaught exception: (illegal-arguments (14 23 57))

    | (robin (0 1) ((arith (0 1) *))
    |   (/ 14 #t))
    ? uncaught exception: (expected-number #t)

    | (robin (0 1) ((arith (0 1) *))
    |   (/ #t 51))
    ? uncaught exception: (expected-number #t)

### `abs` ###

`abs` evaluates its single argument to a number, and evaluates to
the absolute value of that number (where the sign is always positive.)

    | (robin (0 1) ((arith (0 1) *))
    |   (abs 5))
    = 5

    | (robin (0 1) ((arith (0 1) *))
    |   (abs (- 0 5)))
    = 5

    | (robin (0 1) ((arith (0 1) *))
    |   (abs 0))
    = 0

`abs` expects exactly one numeric argument.

    | (robin (0 1) ((arith (0 1) *))
    |   (abs))
    ? uncaught exception: (illegal-arguments ())

    | (robin (0 1) ((arith (0 1) *))
    |   (abs 14 23))
    ? uncaught exception: (illegal-arguments (14 23))

    | (robin (0 1) ((arith (0 1) *))
    |   (abs #t))
    ? uncaught exception: (expected-number #t)

### `frac` ###

`frac` evaluates its single argument to a number, and evaluates to
the fractional portion of that number (i.e., that number, minus the
integer portion of that number.)

    | (robin (0 1) ((arith (0 1) *))
    |   (frac 6/5))
    = 1/5

    | (robin (0 1) ((arith (0 1) *))
    |   (frac 8))
    = 0

The result of `frac` is always positive.

    | (robin (0 1) ((arith (0 1) *))
    |   (frac (- 0 6/5)))
    = 1/5

`frac` expects exactly one numeric argument.

    | (robin (0 1) ((arith (0 1) *))
    |   (frac))
    ? uncaught exception: (illegal-arguments ())

    | (robin (0 1) ((arith (0 1) *))
    |   (frac 14 23))
    ? uncaught exception: (illegal-arguments (14 23))

    | (robin (0 1) ((arith (0 1) *))
    |   (frac #t))
    ? uncaught exception: (expected-number #t)

### `integer?` ###

`integer?` evaluates its argument, then evaluates to `#t` if that
argument is a number without any fractional part, and to `#f` otherwise.

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

The argument to `integer?` must be of numeric type.  TODO: this may
be relaxed in the future.

    | (robin (0 1) ((arith (0 1) *))
    |   (integer? #t))
    ? uncaught exception: (expected-number #t)

`integer?` expects exactly one argument.

    | (robin (0 1) ((arith (0 1)))
    |   (arith:integer?))
    ? uncaught exception: (illegal-arguments ())

    | (robin (0 1) ((arith (0 1)))
    |   (arith:integer? 14 23))
    ? uncaught exception: (illegal-arguments (14 23))

### `natural?` ###

`natural?` evaluates its argument, then evaluates to `#t` if that argument
is a natural number (a non-negative integer) and to `#f` otherwise.

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

The argument to `natural?` must be of numeric type.  TODO: this may
be relaxed in the future.

    | (robin (0 1) ((arith (0 1) *))
    |   (natural? #t))
    ? uncaught exception: (expected-number #t)

`natural?` expects exactly one argument.

    | (robin (0 1) ((arith (0 1)))
    |   (arith:natural?))
    ? uncaught exception: (illegal-arguments ())

    | (robin (0 1) ((arith (0 1)))
    |   (arith:natural? 14 23))
    ? uncaught exception: (illegal-arguments (14 23))

### `div` ###

`div` evaluates both of its arguments to numbers and evaluates to the
result of integer division of the first number by the second.  Integer
division computes by what integer the second number can be multiplied
to make it as big as possible without exceeding the first number.

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

Division by zero is undefined, and an exception will be raised.

    | (robin (0 1) ((arith (0 1)))
    |   (arith:div 10 0))
    ? uncaught exception: (division-by-zero 10)

`div` expects exactly two arguments, both numbers.

    | (robin (0 1) ((arith (0 1) *))
    |   (div 14))
    ? uncaught exception: (illegal-arguments (14))

    | (robin (0 1) ((arith (0 1) *))
    |   (div 14 23 57))
    ? uncaught exception: (illegal-arguments (14 23 57))

    | (robin (0 1) ((arith (0 1) *))
    |   (div 14 #t))
    ? uncaught exception: (expected-number #t)

    | (robin (0 1) ((arith (0 1) *))
    |   (div #t 51))
    ? uncaught exception: (expected-number #t)

### `rem` ###

`rem` evaluates both of its arguments to numbers and evaluates to the
remainder of the division of the first number by the second.

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

Trying to find the remainder of a division by zero is undefined, and an
exception will be raised.

    | (robin (0 1) ((arith (0 1) *))
    |   (rem 10 0))
    ? uncaught exception: (division-by-zero 10)

When the arguments are not whole numbers, the remainder is still
a whole number. TODO: the semantics here need to be cleaned up.

    | (robin (0 1) ((arith (0 1) *))
    |   (rem 10 10/3))
    = 0

    | (robin (0 1) ((arith (0 1) *))
    |   (rem 10/3 3))
    = 1/3

`rem` expects exactly two arguments, both numbers.

    | (robin (0 1) ((arith (0 1) *))
    |   (rem 14))
    ? uncaught exception: (illegal-arguments (14))

    | (robin (0 1) ((arith (0 1) *))
    |   (rem 14 23 57))
    ? uncaught exception: (illegal-arguments (14 23 57))

    | (robin (0 1) ((arith (0 1) *))
    |   (rem 14 #t))
    ? uncaught exception: (expected-number #t)

    | (robin (0 1) ((arith (0 1) *))
    |   (rem #t 51))
    ? uncaught exception: (expected-number #t)

### `>` ###

`>` evaluates both of its arguments to numbers, then evaluates to `#t`
if the first number is strictly greater than the second.

    | (robin (0 1) ((arith (0 1) *))
    |   (> 6 4))
    = #t

    | (robin (0 1) ((arith (0 1) *))
    |   (> 6 8))
    = #f

    | (robin (0 1) ((arith (0 1) *))
    |   (> 6 6))
    = #f

`>` expects exactly two arguments, both numbers.

    | (robin (0 1) ((arith (0 1) *))
    |   (> 14))
    ? uncaught exception: (illegal-arguments (14))

    | (robin (0 1) ((arith (0 1) *))
    |   (> 14 23 57))
    ? uncaught exception: (illegal-arguments (14 23 57))

    | (robin (0 1) ((arith (0 1) *))
    |   (> 14 #t))
    ? uncaught exception: (expected-number #t)

    | (robin (0 1) ((arith (0 1) *))
    |   (> #t 51))
    ? uncaught exception: (expected-number #t)

### `<` ###

`<` evaluates both of its arguments to numbers, then evaluates to `#t`
if the first number is strictly less than the second.

    | (robin (0 1) ((arith (0 1) *))
    |   (< 6 4))
    = #f

    | (robin (0 1) ((arith (0 1) *))
    |   (< 6 8))
    = #t

    | (robin (0 1) ((arith (0 1) *))
    |   (< 6 6))
    = #f

`<` expects exactly two arguments, both numbers.

    | (robin (0 1) ((arith (0 1) *))
    |   (< 14))
    ? uncaught exception: (illegal-arguments (14))

    | (robin (0 1) ((arith (0 1) *))
    |   (< 14 23 57))
    ? uncaught exception: (illegal-arguments (14 23 57))

    | (robin (0 1) ((arith (0 1) *))
    |   (< 14 #t))
    ? uncaught exception: (expected-number #t)

    | (robin (0 1) ((arith (0 1) *))
    |   (< #t 51))
    ? uncaught exception: (expected-number #t)

### `>=` ###

`>=` evaluates both of its arguments to numbers, then evaluates to `#t`
if the first number is greater than or equal to the second.

    | (robin (0 1) ((arith (0 1) *))
    |   (>= 6 4))
    = #t

    | (robin (0 1) ((arith (0 1) *))
    |   (>= 6 8))
    = #f

    | (robin (0 1) ((arith (0 1) *))
    |   (>= 6 6))
    = #t

`>=` expects exactly two arguments, both numbers.

    | (robin (0 1) ((arith (0 1) *))
    |   (>= 14))
    ? uncaught exception: (illegal-arguments (14))

    | (robin (0 1) ((arith (0 1) *))
    |   (>= 14 23 57))
    ? uncaught exception: (illegal-arguments (14 23 57))

    | (robin (0 1) ((arith (0 1) *))
    |   (>= 14 #t))
    ? uncaught exception: (expected-number #t)

    | (robin (0 1) ((arith (0 1) *))
    |   (>= #t 51))
    ? uncaught exception: (expected-number #t)

### `<=` ###

`<=` evaluates both of its arguments to numbers, then evaluates to `#t`
if the first number is less than or equal to the second.

    | (robin (0 1) ((arith (0 1) *))
    |   (<= 6 4))
    = #f

    | (robin (0 1) ((arith (0 1) *))
    |   (<= 6 8))
    = #t

    | (robin (0 1) ((arith (0 1) *))
    |   (<= 6 6))
    = #t

`<=` expects exactly two arguments, both numbers.

    | (robin (0 1) ((arith (0 1) *))
    |   (<= 14))
    ? uncaught exception: (illegal-arguments (14))

    | (robin (0 1) ((arith (0 1) *))
    |   (<= 14 23 57))
    ? uncaught exception: (illegal-arguments (14 23 57))

    | (robin (0 1) ((arith (0 1) *))
    |   (<= 14 #t))
    ? uncaught exception: (expected-number #t)

    | (robin (0 1) ((arith (0 1) *))
    |   (<= #t 51))
    ? uncaught exception: (expected-number #t)

### `ascending?` ###

    | (robin (0 1) ((arith (0 1) *) (small (0 1) *))
    |   (ascending? (list 1 2 3)))
    = #t

    | (robin (0 1) ((arith (0 1) *) (small (0 1) *))
    |   (ascending? (list 1 2 2 3)))
    = #t

    | (robin (0 1) ((arith (0 1) *) (small (0 1) *))
    |   (ascending? (list 3 2 1)))
    = #f

    | (robin (0 1) ((arith (0 1) *) (small (0 1) *))
    |   (ascending? (list 1 2 1 3)))
    = #f

    | (robin (0 1) ((arith (0 1) *) (small (0 1) *))
    |   (ascending? ()))
    = #t

    | (robin (0 1) ((arith (0 1) *) (small (0 1) *))
    |   (ascending? (list 100)))
    = #t

    | (robin (0 1) ((arith (0 1) *) (small (0 1) *))
    |   (ascending? 44))
    ? uncaught exception: (expected-list 44)

### `strictly-ascending?` ###

    | (robin (0 1) ((arith (0 1) *) (small (0 1) *))
    |   (strictly-ascending? (list 1 2 3)))
    = #t

    | (robin (0 1) ((arith (0 1) *) (small (0 1) *))
    |   (strictly-ascending? (list 1 2 2 3)))
    = #f

    | (robin (0 1) ((arith (0 1) *) (small (0 1) *))
    |   (strictly-ascending? (list 1 2 1 3)))
    = #f

    | (robin (0 1) ((arith (0 1) *) (small (0 1) *))
    |   (strictly-ascending? ()))
    = #t

    | (robin (0 1) ((arith (0 1) *) (small (0 1) *))
    |   (strictly-ascending? (list 100)))
    = #t

    | (robin (0 1) ((arith (0 1) *) (small (0 1) *))
    |   (strictly-ascending? 44))
    ? uncaught exception: (expected-list 44)

### `descending?` ###

    | (robin (0 1) ((arith (0 1) *) (small (0 1) *))
    |   (descending? (list 3 2 1)))
    = #t

    | (robin (0 1) ((arith (0 1) *) (small (0 1) *))
    |   (descending? (list 3 3 3 2 2 1)))
    = #t

    | (robin (0 1) ((arith (0 1) *) (small (0 1) *))
    |   (descending? (list 1 2 3)))
    = #f

    | (robin (0 1) ((arith (0 1) *) (small (0 1) *))
    |   (descending? (list 3 2 3 1)))
    = #f

    | (robin (0 1) ((arith (0 1) *) (small (0 1) *))
    |   (descending? ()))
    = #t

    | (robin (0 1) ((arith (0 1) *) (small (0 1) *))
    |   (descending? (list 100)))
    = #t

    | (robin (0 1) ((arith (0 1) *) (small (0 1) *))
    |   (descending? 44))
    ? uncaught exception: (expected-list 44)

### `strictly-descending?` ###

    | (robin (0 1) ((arith (0 1) *) (small (0 1) *))
    |   (strictly-descending? (list 3 2 1)))
    = #t

    | (robin (0 1) ((arith (0 1) *) (small (0 1) *))
    |   (strictly-descending? (list 3 2 2 1)))
    = #f

    | (robin (0 1) ((arith (0 1) *) (small (0 1) *))
    |   (strictly-descending? (list 3 2 3 1)))
    = #f

    | (robin (0 1) ((arith (0 1) *) (small (0 1) *))
    |   (strictly-descending? ()))
    = #t

    | (robin (0 1) ((arith (0 1) *) (small (0 1) *))
    |   (strictly-descending? (list 100)))
    = #t

    | (robin (0 1) ((arith (0 1) *) (small (0 1) *))
    |   (strictly-descending? 44))
    ? uncaught exception: (expected-list 44)
