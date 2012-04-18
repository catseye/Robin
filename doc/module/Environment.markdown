Module `env`
============

    -> Tests for functionality "Interpret Robin Program"

The `env` module exports macros for examining and manipulating evaluation
environments and, to the extent they are represented as binding alists,
binding alists.

### `env?` ###

`env?` evaluates its single argument, and evaluates to `#t` if
and only if it is a well-formed binding alist.

    | (robin (0 1) ((small (0 1) *) (env (0 1) *))
    |   (env? (literal ((a 1) (b 2) (c 3)))))
    = #t

    | (robin (0 1) ((small (0 1) *) (env (0 1) *))
    |   (env? (literal ((a 1) (999 2) (c 3)))))
    = #f

    | (robin (0 1) ((small (0 1) *) (env (0 1) *))
    |   (env? (literal ((a 1) (b 2) c))))
    = #f

    | (robin (0 1) ((small (0 1) *) (env (0 1) *))
    |   (env? 7))
    = #f

    | (robin (0 1) ((small (0 1) *) (env (0 1) *))
    |   (env? (env)))
    = #t

### `unbind` ###

`unbind` removes the given identifier from the environment and evaluates its
second argument in that reduced environment.

    | (robin (0 1) ((small (0 1) *) (env (0 1) *))
    |   (unbind if (if #t (literal x) (literal y))))
    ? uncaught exception: (unbound-identifier if)

If the identifier doesn't exist in the environment, no change is made to
the environment.

    | (robin (0 1) ((small (0 1) *) (env (0 1) *))
    |   (unbind yog-sothoth (if #t (literal x) (literal y))))
    = x

`unbind` removes all trace of binding from the given identifier; if that
identifier has several definitions that are shadowed, none of them will be
in effect.

    | (robin (0 1) ((small (0 1) *) (env (0 1) *))
    |   (let ((x 7))
    |     (let ((x 8))
    |       (unbind x
    |         x))))
    ? uncaught exception: (unbound-identifier x)

### `sandbox` ###

`sandbox` takes a list of identifiers as its first argument, and evaluates
its second argument in an environment where all bindings *except* those
for the listed identifiers have been unbound.

    | (robin (0 1) ((core (0 1) *) (env (0 1) *))
    |   (sandbox (pair tail)
    |     (tail (pair 8 (pair 9 ())))))
    = (9)

    | (robin (0 1) ((core (0 1) *) (env (0 1) *))
    |   (sandbox (pair tail)
    |     (head (pair 8 (pair 9 ())))))
    ? uncaught exception: (unbound-identifier head)

### `export` ###

`export` treats its arguments as a list of identifiers, and returns an
environment where only those identifiers are bound to values.

The original idea for `sandbox` was that it could be used in the body of
a module to restrict the visible identifiers to those the module wished
to export, which could then actually be exported with `env`.  However,
this still required `env` to be a visible identifier (and thus exported.)
`export` simply evaluates to a binding alist which can be returned
directly.

Note: the order of the bindings in the binding alist isn't guaranteed;
these tests should be rewritten to search the resulting alist.

    | (robin (0 1) ((small (0 1) *) (env (0 1) *))
    |   (let ((a 1) (b 2))
    |     (export a b)))
    = ((b 2) (a 1))

    | (robin (0 1) ((small (0 1) *) (env (0 1) *))
    |   (export head tail))
    = ((head (builtin head)) (tail (builtin tail)))

### `unshadow` ###

`unshadow` is similar to `unbind`, but only removes the latest binding
for the given identifier; previously shadowed bindings, if any exist,
will be visible instead.

    | (robin (0 1) ((small (0 1) *) (env (0 1) *))
    |   (unshadow yog-sothoth (if #t (literal x) (literal y))))
    = x

    | (robin (0 1) ((small (0 1) *) (env (0 1) *))
    |   (unshadow if (if #t (literal x) (literal y))))
    ? uncaught exception: (unbound-identifier if)

    | (robin (0 1) ((small (0 1) *) (env (0 1) *))
    |   (bind if (literal what)
    |     (unshadow if (if #t (literal x) (literal y)))))
    = x

    | (robin (0 1) ((small (0 1) *) (env (0 1) *))
    |   (bind q 400
    |     (unshadow q q)))
    ? uncaught exception: (unbound-identifier q)

    | (robin (0 1) ((small (0 1) *) (env (0 1) *))
    |   (bind q 200
    |     (bind q 400
    |       (unshadow q q))))
    = 200

    | (robin (0 1) ((small (0 1) *) (env (0 1) *))
    |   (bind q 100
    |     (bind q 200
    |       (bind q 400
    |         (unshadow q (unshadow q q))))))
    = 100

    | (robin (0 1) ((small (0 1) *) (env (0 1) *))
    |   (let ((q 100)
    |         (q 200)
    |         (q 400))
    |     (unshadow q (unshadow q q))))
    = 100

`unshadow` is something of a gimmick that shows off Robin's ability
to manipulate the evaluation environment.  In practice, the bindings
can be determined lexically, and a different identifier could always
be chosen instead.

### Re-exported Functions ###

Because it would be reasonable to find them here by categorization,
this module re-exports the macro `env` from `core`, and `bind` and `let`
from `small`.

| (robin (0 1) ((env (0 1)))
|   (env:env? (env:env)))
= #t

| (robin (0 1) ((env (0 1)))
|   (env:bind a 1 a))
= 1

| (robin (0 1) ((env (0 1)))
|   (env:let ((a 7) (b a)) b))
= 7
