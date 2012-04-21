Module `small`
==============

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

Note that, for the purpose of simplicity of definition, the behavior of many
of these macros differ from the more usual behavior of raising an
`illegal-arguments` exception, when the arguments supplied in the macro
call are not satisfactory.  In most cases, an exception will be raised,
but exactly which exception that is, is not specified.  In some cases,
extra arguments to the macro will be ignored and discarded.  A static
analyzer may be provided one day which detects these cases and raises an
exception, but that is a matter of static analysis, not execution.

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

`literal` requires at least one argument; otherwise, an exception will
be raised.

    | (robin (0 1) ((small (0 1) *))
    |   (literal))
    ? uncaught exception

Any arguments beyond the first argument are simply ignored and discarded.

    | (robin (0 1) ((small (0 1) *))
    |   (literal a b c))
    = a

`literal` is basically equivalent to Scheme's `quote`.

### `list` ###

    | (robin (0 1) ((small (0 1)))
    |   (small:list 1 2 3))
    = (1 2 3)

Unlike `literal`, `list` does evaluate its arguments, all of them.

    | (robin (0 1) ((small (0 1) *))
    |   (list (literal x) (literal y)))
    = (x y)

`list` does not require any arguments.

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

But, a function must have exactly both a body and a list of formal arguments.
Otherwise, an exception will be raised.

    | (robin (0 1) ((small (0 1) *))
    |   ((fun ())))
    ? uncaught exception

    | (robin (0 1) ((small (0 1) *))
    |   ((fun)))
    ? uncaught exception

    | (robin (0 1) ((small (0 1) *))
    |   ((fun (a) a a)))
    ? uncaught exception

An `illegal-arguments` exception will be raised if not enough arguments are
supplied to a function call.

    | (robin (0 1) ((small (0 1) *))
    |   ((fun (a b) (list b a))
    |     (prepend 1 ())))
    ? uncaught exception: (illegal-arguments

An `illegal-arguments` exception will be raised if too many arguments are
supplied to a function call.

    | (robin (0 1) ((small (0 1) *))
    |   ((fun (a b) (list b a))
    |     1 (prepend 2 ()) 3))
    ? uncaught exception: (illegal-arguments

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

`bind` expects exactly three arguments, or else an exception will be raised.

    | (robin (0 1) ((small (0 1) *))
    |   (bind smoosh (fun (x y) (list y x))))
    ? uncaught exception

    | (robin (0 1) ((small (0 1) *))
    |   (bind smoosh))
    ? uncaught exception

    | (robin (0 1) ((small (0 1) *))
    |   (bind))
    ? uncaught exception

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

The list of bindings must be a list, or else an exception will be raised.

    | (robin (0 1) ((small (0 1) *))
    |   (let 999 (literal hi)))
    ? uncaught exception

Each binding in a list must be a list, or else an exception will be raised.

    | (robin (0 1) ((small (0 1) *))
    |   (let (999) (literal hi)))
    ? uncaught exception

Both the body and the list of bindings are required, or else an exception
will be raised.

    | (robin (0 1) ((small (0 1) *))
    |   (let ()))
    ? uncaught exception

    | (robin (0 1) ((small (0 1) *))
    |   (let))
    ? uncaught exception

Any arguments given beyond the body and list of bindings will be ignored
and discarded, without being evaluated.

    | (robin (0 1) ((small (0 1) *))
    |   (let ((a 1)) a foo))
    = 1

Each binding must have at least a name and a value, or else an exception
will be raised.
    
    | (robin (0 1) ((small (0 1) *))
    |   (let ((a)) a))
    ? uncaught exception

    | (robin (0 1) ((small (0 1) *))
    |   (let (()) 7))
    ? uncaught exception

Anything given in a binding beyond the name and the value will simply be
ignored and discarded, without being evaluated or otherwise examined.

    | (robin (0 1) ((small (0 1) *))
    |   (let ((a 1 foo)) a))
    = 1

The identifier in a binding must be a symbol.

    | (robin (0 1) ((small (0 1) *))
    |   (let ((3 1)) 3))
    ? uncaught exception: (illegal-binding (3 1))

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
    
`choose` does require an `else` branch, or else an exception will be
raised.

    | (robin (0 1) ((small (0 1) *))
    |   (choose (#f (literal hi)) (#f (literal med))))
    ? uncaught exception

    | (robin (0 1) ((small (0 1) *))
    |   (choose))
    ? uncaught exception

Each branch of a `choose` needs to be a two-element list, or else an
exception will be raised.

    | (robin (0 1) ((small (0 1) *))
    |   (choose (#t) (else (literal lo))))
    ? uncaught exception

    | (robin (0 1) ((small (0 1) *))
    |   (choose (#f 66) (else)))
    ? uncaught exception

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

`env` expects no arguments.  Any arguments supplied will be simply ignored
and discarded, without being evaluated.

    | (robin (0 1) ((small (0 1) *))
    |   (bind find (fun (self alist key)
    |                 (if (equal? alist (literal ())) (literal ())
    |                    (if (equal? key (head (head alist)))
    |                       (head alist)
    |                       (self self (tail alist) key))))
    |     (prepend
    |       (find find (env find) (literal boolean?))
    |       (find find (env (goofah whatever)) (literal prepend)))))
    = ((boolean? (builtin boolean?)) prepend (builtin prepend))
