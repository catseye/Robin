Module `small`
==============

    -> Functionality "Interpret Robin Program" is implemented by
    -> shell command "bin/robin -B small %(test-file)"

    -> Tests for functionality "Interpret Robin Program"

The `core` module only exports macros which are necessarily, or for reasons
of practicality, not implemented in Robin itself.  For example, in the Robin
reference interpreter, they are implemented in Haskell.

This small set of macros omits many abstractions to which programmers have
become accustomed, and is thus rather brutal to program directly in.

So to make Robin somewhat easier to program in, the `small` module exports a
number of macros which help bring the language up to parity with Pixley.

That is, the amount of functionality in `small` is rather modest -- only a
fraction of what you would find in the Haskell standard prelude, or in R5RS
Scheme, or in the Python core.

All of these macros can be written in core Robin, but whether they are, or
provided as builtins, is up to the implementation.

In addition, the `small` module re-exports everything from `core`, so that
it is not necessary to import both of these modules, only `small`.

### `literal` ###

One of the most basic identifiers available in `small` is `literal`,
which evaluates to the literal content of its sole argument, which can be
any S-expression.

    | (robin (0 1) ((small (0 1) *))
    |   (literal symbol))
    = symbol

    | (robin (0 1) ((small (0 1) *))
    |   (literal (hello (there) world)))
    = (hello (there) world)

`literal` is basically equivalent to Scheme's `quote`.

### `list` ###

    | (robin (0 1) ((small (0 1)))
    |   (small:list 1 2 3))
    = (1 2 3)

    | (robin (0 1) ((small (0 1) *))
    |   (list (literal x) (literal y)))
    = (x y)

    | (robin (0 1) ((small (0 1) *))
    |   (list))
    = ()

### `fun` ###

You can define functions with `fun`.  They can be anonymous.

    | (robin (0 1) ((small (0 1) *))
    |   ((fun (a) a) (literal whee)))
    = whee

Function have "closure" behavior; that is, bindings in force when a
function is defined will still be in force when the function is applied,
even if they are no longer lexically in scope.

    | (robin (0 1) ((small (0 1) *))
    |   ((let
    |      ((a (literal (hi)))
    |       (f (fun (x) (list x a))))
    |     f) (literal oh)))
    = (oh (hi))

Functions can take functions.

    | (robin (0 1) ((small (0 1) *))
    |   (let
    |     ((apply (fun (x) (x (literal a)))))
    |     (apply (fun (r) (list r)))))
    = (a)

Functions can return functions.

    | (robin (0 1) ((small (0 1) *))
    |   (let
    |     ((mk (fun (x) (fun (y) (prepend y x))))
    |      (mk2 (mk (literal (vindaloo)))))
    |     (mk2 (literal chicken))))
    = (chicken vindaloo)

Arguments to functions shadow any other bindings in effect.

    | (robin (0 1) ((small (0 1) *))
    |   (let
    |     ((a (literal a))
    |      (b (fun (a) (list a a))))
    |     (b 7)))
    = (7 7)

A function may have no arguments at all.

    | (robin (0 1) ((small (0 1) *))
    |   ((fun () 7)))
    = 7

An exception will be raised if not enough arguments are supplied to a
function call.

    | (robin (0 1) ((small (0 1) *))
    |   ((fun (a b) (list b a))
    |     (prepend 1 ())))
    ? uncaught exception: (illegal-arguments ((prepend 1 ()))

An exception will be raised if too many arguments are supplied to a
function call.

    | (robin (0 1) ((small (0 1) *))
    |   ((fun (a b) (list b a))
    |     1 (prepend 2 ()) 3))
    ? uncaught exception: (illegal-arguments (1 (prepend 2 ()) 3))

`fun` is basically equivalent to Scheme's `lambda`.

### `bind` ###

`bind` binds a single identifier to the result of evaluating a single
expression, and makes that binding available in another expression which
it evaluates.

    | (robin (0 1) ((small (0 1) *))
    |   (bind x (literal hello)
    |     (list x x)))
    = (hello hello)

    | (robin (0 1) ((small (0 1) *))
    |   (bind dup (fun (x) (list x x))
    |     (dup (literal g))))
    = (g g)

    | (robin (0 1) ((small (0 1) *))
    |   (bind dup (fun (x) (list x x))
    |     (dup (dup (literal g)))))
    = ((g g) (g g))

    | (robin (0 1) ((small (0 1) *))
    |   (bind smoosh (fun (x y) (list y x))
    |     (smoosh #t #f)))
    = (#f #t)

    | (robin (0 1) ((small (0 1) *))
    |   (bind find (fun (self alist key)
    |                 (if (equal? alist (literal ())) (literal ())
    |                    (if (equal? key (head (head alist)))
    |                       (head alist)
    |                       (self self (tail alist) key))))
    |     (find find (literal ((c d) (e f) (a b))) (literal a))))
    = (a b)

`bind` is basically equivalent to Scheme's `let`, but only one
binding may be given.

### `let` ###

`let` lets you bind multiple identifiers to multiple values.

An identifier can be bound to a symbol.

    | (robin (0 1) ((small (0 1) *))
    |   (let ((a (literal hello))) a))
    = hello

`let` can appear in the binding expression in a `let`.

    | (robin (0 1) ((small (0 1) *))
    |   (let ((a (let ((b (literal c))) b))) a))
    = c

`let` can bind a symbol to a function value.

    | (robin (0 1) ((small (0 1) *))
    |   (let ((a (fun (x y) (prepend y x))))
    |         (a () (literal foo))))
    = (foo)

Bindings established in a `let` remain in effect when evaluating
the arguments things in the body of the `let`.

    | (robin (0 1) ((small (0 1) *))
    |   (let ((dup (fun (x) (list x x))))
    |     (dup (dup (literal g)))))
    = ((g g) (g g))

Bindings established in a binding in a `let` can be seen in
subsequent bindings in the same `let`.

    | (robin (0 1) ((small (0 1) *))
    |   (let ((a (literal hello)) (b (list a))) b))
    = (hello)

Shadowing happens.

    | (robin (0 1) ((small (0 1) *))
    |   (let ((a (literal hello))) (let ((a (literal goodbye))) a)))
    = goodbye

`let` can have an empty list of bindings.

    | (robin (0 1) ((small (0 1) *))
    |   (let () (literal hi)))
    = hi

`let` is basically equivalent to Scheme's `let*` or Haskell's `let`.

### `choose` ###

`choose` expects to be given a list of tests.  Each test is a two-element
list, the first element of which is a condition which should evaluate to
a boolean, and the second element of which is an expression, which
will be evaluated only if the boolean is `#t`, and `choose` will immediately
evaluate to that result without trying any of the subsequent tests.  The
condition in the final test must be the literal symbol `else`; the
corresponding expression will be evaluated if all other tests failed.

    | (robin (0 1) ((small (0 1) *))
    |   (choose (#t (literal hi)) (else (literal lo))))
    = hi

    | (robin (0 1) ((small (0 1) *))
    |   (choose (#f (literal hi)) (#t (literal med)) (else (literal lo))))
    = med

    | (robin (0 1) ((small (0 1) *))
    |   (choose (#f (literal hi)) (#f (literal med)) (else (literal lo))))
    = lo

`choose` can have zero tests before the `else`.

    | (robin (0 1) ((small (0 1) *))
    |   (choose (else (literal woo))))
    = woo

`choose` is basically equivalent to Scheme's `cond`.

### `env` ###

`env` evaluates to all the bindings in effect at the point of execution
where this form is encountered, as an alist.

    | (robin (0 1) ((small (0 1) *))
    |   (bind find (fun (self alist key)
    |                 (if (equal? alist (literal ())) (literal ())
    |                    (if (equal? key (head (head alist)))
    |                       (head alist)
    |                       (self self (tail alist) key))))
    |     (prepend
    |       (find find (env) (literal boolean?)) (find find (env) (literal prepend)))))
    = ((boolean? (builtin boolean?)) prepend (builtin prepend))
