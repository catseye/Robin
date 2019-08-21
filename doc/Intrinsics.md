Robin Intrinsics
================

    -> Tests for functionality "Interpret core Robin Program"

An _intrinsic_ is one of the data types in Robin.  It is like a macro, except
that it is implemented intrinsically (and thus does not support quite
every operation that is supported on macros, for example, examining its
internals.)

Robin 0.3 provides 15 intrinsics.  These represent
the fundamental functionality that is used to evaluate programs, and that
cannot be expressed as macros written in Robin (not without resorting to
meta-circularity, at any rate.)  All other macros are built up on top of
the intrinsics.

This set of intrinsics is not optional — every Robin implementation must
provide them, or it's not Robin.

Intrinsics usually have undefined behaviour if their preconditions are
not met (i.e., if they are called with wrong number or types of arguments.)
Obviously, we can't write tests for those cases here.  However, for each
intrinsics, there is a corresponding macro in `stdlib` which wraps the
intrinsics, and is named the same except omitting the ``.  These wrappers
give the intrinsics predictable failure modes in these cases, by raising
defined exceptions.

### `prepend` ###

`prepend` evaluates both of its arguments, then evaluates to a list cell
which contains the first value as its data and the second value as the
continuation of the list.

    | (display
    |   (prepend () ()))
    = (())

    | (display
    |   (prepend #t (prepend #f ())))
    = (#t #f)

`prepend` expects exactly two arguments.  The first may be of any type.
The second`prepend` must be a list.  If these conditions are not met,
the behaviour is undefined.

`prepend` is basically equivalent to Scheme's `cons`, except for the
requirement that the second argument be a list.

### `head` ###

`head` evaluates its argument to a list, and evaluates to the first element
of that list.

    | (display
    |   (head (prepend #t ())))
    = #t

`head` expects exactly one argument, and expects it to be a list.
If these conditions are not met, the behaviour is undefined.

`head` is basically equivalent to Scheme's `car`.

### `tail` ###

`tail` evaluates its argument to a list, and evaluates to the tail of that
list (the sublist obtained by removing the first element.)

    | (display
    |   (tail (prepend #t (prepend #f ()))))
    = (#f)

`tail` expects exactly one argument, and expects it to be a list.
If these conditions are not met, the behaviour is undefined.

`tail` is basically equivalent to Scheme's `cdr`.

### `if` ###

`if` evaluates its first argument to a boolean value.  If that value is
`#t`, it evaluates, and evaluates to, its second argument; or if that value
is `#f` it evaluates, and evaluates to, its third argument.  In all cases,
at most two arguments are evaluated.

    | (display
    |   (if #t 7 9))
    = 7

    | (display
    |   (if #f 7 9))
    = 9

The identifiers named in the branch which is not evaluated need not be
properly bound to values in the environment.

    | (display
    |   (if #t 1 (prepend fred ethel)))
    = 1

The second and third arguments can be arbitrary expressions, but `if`
expects its first argument to be a boolean.  `if` expects exactly three
arguments.  If these conditions are not met, the behaviour is undefined.

`if` is basically equivalent to Scheme's `if`.

### `equal?` ###

`equal?` evaluates both of its arguments to arbitrary S-expressions
and compares them for deep equality.

`equal?` works on symbols.

    | (define literal (macro (s a e) (head a)))
    | (display
    |   (equal?
    |     (literal this-symbol)
    |     (literal this-symbol)))
    = #t

    | (define literal (macro (s a e) (head a)))
    | (display
    |   (equal?
    |     (literal this-symbol)
    |     (literal that-symbol)))
    = #f

`equal?` works on lists.

    | (display
    |   (equal? (prepend 1 (prepend 2 (prepend 3 ())))
    |           (prepend 1 (prepend 2 (prepend 3 ())))))
    = #t

`equal?` works on lists, deeply.

    | (display
    |   (equal? (prepend 1 (prepend 2 (prepend 7 ())))
    |           (prepend 1 (prepend 2 (prepend 3 ())))))
    = #f

Two values of different types are never equal.

    | (define literal (macro (s a e) (head a)))
    | (display
    |   (equal? #t
    |            (prepend (literal a) ())))
    = #f

    | (display
    |   (equal? #f
    |           ()))
    = #f

`equal?` expects exactly two arguments, of any type.

### `list?` ###

`list?` evaluates its argument, then evaluates to `#t` if it is a list,
`#f` otherwise.

    | (define literal (macro (s a e) (head a)))
    | (display
    |   (list? (literal (a b))))
    = #t

    | (define literal (macro (s a e) (head a)))
    | (display
    |   (list? (literal (a b c d e f))))
    = #t

    | (display
    |   (list? (prepend 4 (prepend 5 ()))))
    = #t

The empty list is a list.

    | (display
    |   (list? ()))
    = #t

Symbols are not lists.

    | (define literal (macro (s a e) (head a)))
    | (display
    |   (list? (literal a)))
    = #f

The argument to `list?` may (naturally) be any type, but there must be
exactly one argument.

### `macro?` ###

`macro?` evaluates its argument, then evaluates to `#t` if it is a macro,
or `#f` if it is not.

    | (display
    |   (macro? (macro (self args env) args)))
    = #t

TODO: this should probably be false.  Intrinsics are slightly different
from macros.  Either that, or, it should be, like `applyable?`, or
something.

    | (display
    |   (macro? macro))
    = #t

    | (display
    |   (macro? ((macro (self args env) (head args)) macro)))
    = #f

    | (display
    |   (macro? 5))
    = #f

The argument to `macro?` may (naturally) be any type, but there must be
exactly one argument.

### `symbol?` ###

`symbol?` evaluates its argument, then evaluates to `#t` if it is a symbol,
`#f` otherwise.

    | (define literal (macro (s a e) (head a)))
    | (display
    |   (symbol? (literal this-symbol)))
    = #t

Numbers are not symbols.

    | (display
    |   (symbol? 9))
    = #f

Lists are not symbols.

    | (display
    |   (symbol? (prepend 1 ())))
    = #f

The argument to `symbol?` may (naturally) be any type, but there must be
exactly one argument.

### `number?` ###

`number?` evaluates its argument, then evaluates to `#t` if it is a
number, `#f` otherwise.

    | (display
    |   (number? 7))
    = #t

    | (display
    |   (number? 0))
    = #t

    | (display
    |   (number? ()))
    = #f

    | (display
    |   (number? #t))
    = #f

    | (define literal (macro (s a e) (head a)))
    | (display
    |   (number? (literal seven)))
    = #f

That's a good question...

    | (define literal (macro (s a e) (head a)))
    | (display
    |   (number? (literal 7)))
    = #t

The argument to `number?` may (naturally) be any type, but there must be
exactly one argument.

### `subtract` ###

`subtract` evaluates its first argument to a number, then
evaluates its second argument to a number, then evaluates
to the difference between the first and second numbers.

    | (display
    |   (subtract 6 4))
    = 2

    | (display
    |   (subtract 1000 8000))
    = -7000

Addition may be accomplished by negating the second argument.

    | (display
    |   (subtract 999 (subtract 0 999)))
    = 1998

`subtract` expects both of its arguments to be numbers.

`subtract` expects exactly two arguments.

### `sign` ###

`sign` evaluates its sole argument to a number, then
evaluates to 0 if that number is 0, 1 if that number is positive, or
-1 if that number is negative.

    | (display
    |   (sign 26))
    = 1

    | (display
    |   (sign 0))
    = 0

    | (display
    |   (sign (subtract 0 200)))
    = -1

`sign` expects exactly one argument.

### `eval` ###

`eval` evaluates its first argument to obtain an environment, then
evaluates its second argument to obtain an S-expression; it then
evaluates that S-expression in the given environment.

    | (define literal (macro (s a e) (head a)))
    | (define env (macro (s a e) e))
    | (display
    |   (eval (env) (literal
    |     (prepend (literal a)
    |               (prepend (literal b) ())))))
    = (a b)

    | (define literal (macro (s a e) (head a)))
    | (display
    |   (eval () (literal
    |     (prepend (literal a)
    |               (prepend (literal b) ())))))
    ? uncaught exception: (unbound-identifier prepend)

Something fairly complicated that uses `bind`...?

    | (define literal (macro (s a e) (head a)))
    | (define bind (macro (self args env)
    |   (eval
    |     (prepend (prepend (head args) (prepend (eval env (head (tail args))) ())) env)
    |     (head (tail (tail args))))))
    | (display
    |   (bind bindings (prepend
    |                    (prepend (literal same) (prepend equal? ()))
    |                    (prepend
    |                      (prepend (literal x) (prepend #f ()))
    |                      ()))
    |     (eval bindings (literal (same x x)))))
    = #t

If two bindings for the same identifier are supplied in the environment
alist passed to `eval`, the one closer to the front of the alist takes
precedence.

    | (define literal (macro (s a e) (head a)))
    | (define bind (macro (self args env)
    |   (eval
    |     (prepend (prepend (head args) (prepend (eval env (head (tail args))) ())) env)
    |     (head (tail (tail args))))))
    | (display
    |   (bind bindings (prepend
    |                    (prepend (literal foo) (prepend (literal yes) ()))
    |                    (prepend
    |                       (prepend (literal foo) (prepend (literal no) ()))
    |                       ()))
    |     (eval bindings (literal foo))))
    = yes

### `macro` ###

`macro` takes its first argument to be a list of three formal
parameters, and its second argument to be an arbitrary expression,
and uses these two arguments to build, and evaluate to, a macro
value.

When this macro value is evaluated, the first formal argument will
be bound to the macro itself, the second will be bound to the
literal, unevaluated list of arguments passed to the macro, and the
third will be bound to an alist representing the environment in
effect at the point the macro value is evaluated.

These formals are conventionally called `self`, `args`, and `env`,
but different names can be chosen in the `macro` definition, for
instance to avoid shadowing.

`literal`, in fact, can be defined as a macro, and it is one of the
simplest possible macros that can be written:

    | (display
    |   ((macro (self args env) (head args)) (why hello there)))
    = (why hello there)

And when we want to use it in the tests, we'll define it first, like
this:

    (define literal (macro (s a e) (head a)))

Another facility that can be defined simply by a macro is `env`,
and we'll define it like this:

    (define env (macro (s a e) e))

Macros have "closure" behavior; that is, bindings in force when a
macro is defined will still be in force when the macro is applied,
even if they are no longer lexically in scope.  (Please try to ignore
the heavy `define`s that are used in this test...)

    | (define literal (macro (s a e) (head a)))
    | (define bind (macro (self args env)
    |   (eval
    |     (prepend (prepend (head args) (prepend (eval env (head (tail args))) ())) env)
    |     (head (tail (tail args))))))
    | (define let (macro (self args env)
    |   (bind bindings (head args)
    |     (if (equal? bindings ())
    |       (eval env (head (tail args)))
    |       (bind binding (head bindings)
    |         (bind name (head binding)
    |           (if (symbol? name)
    |             (bind value (eval env (head (tail binding)))
    |               (bind newenv (prepend (prepend name (prepend value ())) env)
    |                 (bind newbindings (tail bindings)
    |                   (bind newargs (prepend newbindings (tail args))
    |                     (eval newenv (prepend self newargs))))))
    |             (raise (prepend (literal illegal-binding) (prepend binding ()))))))))))
    | (display
    |   ((let
    |      ((a (literal these-are))
    |       (m (macro (self args env) (prepend a args))))
    |     m) my args))
    = (these-are my args)

Macros can return macros.

    | (define literal (macro (s a e) (head a)))
    | (define bind (macro (self args env)
    |   (eval
    |     (prepend (prepend (head args) (prepend (eval env (head (tail args))) ())) env)
    |     (head (tail (tail args))))))
    | (define let (macro (self args env)
    |   (bind bindings (head args)
    |     (if (equal? bindings ())
    |       (eval env (head (tail args)))
    |       (bind binding (head bindings)
    |         (bind name (head binding)
    |           (if (symbol? name)
    |             (bind value (eval env (head (tail binding)))
    |               (bind newenv (prepend (prepend name (prepend value ())) env)
    |                 (bind newbindings (tail bindings)
    |                   (bind newargs (prepend newbindings (tail args))
    |                     (eval newenv (prepend self newargs))))))
    |             (raise (prepend (literal illegal-binding) (prepend binding ()))))))))))
    | (display
    |   (let
    |     ((mk (macro (self argsa env)
    |         (macro (self argsb env)
    |           (prepend (head argsb) argsa))))
    |      (mk2 (mk vindaloo)))
    |     (mk2 chicken)))
    = (chicken vindaloo)

Arguments to macros shadow any other bindings in effect.

    | (define literal (macro (s a e) (head a)))
    | (define bind (macro (self args env)
    |   (eval
    |     (prepend (prepend (head args) (prepend (eval env (head (tail args))) ())) env)
    |     (head (tail (tail args))))))
    | (define let (macro (self args env)
    |   (bind bindings (head args)
    |     (if (equal? bindings ())
    |       (eval env (head (tail args)))
    |       (bind binding (head bindings)
    |         (bind name (head binding)
    |           (if (symbol? name)
    |             (bind value (eval env (head (tail binding)))
    |               (bind newenv (prepend (prepend name (prepend value ())) env)
    |                 (bind newbindings (tail bindings)
    |                   (bind newargs (prepend newbindings (tail args))
    |                     (eval newenv (prepend self newargs))))))
    |             (raise (prepend (literal illegal-binding) (prepend binding ()))))))))))
    | (display
    |   (let
    |     ((args (literal a))
    |      (b (macro (self args env) (prepend args args))))
    |     (b 7)))
    = ((7) 7)

`self` is there to let you write recursive macros.  The following
example demonstrates this; it evaluates `(prepend b d)` in an environment
where all the identifiers you list after `qqq` have been bound to 0.

    | (define literal (macro (s a e) (head a)))
    | (define bind (macro (self args env)
    |   (eval
    |     (prepend (prepend (head args) (prepend (eval env (head (tail args))) ())) env)
    |     (head (tail (tail args))))))
    | (display
    |   (bind qqq
    |     (macro (self args env)
    |       (if (equal? args ())
    |         (eval env (literal (prepend b (prepend d ()))))
    |         (eval (prepend (prepend (head args) (prepend 0 ())) env)
    |           (prepend self (tail args)))))
    |     (bind b 1 (bind d 4 (qqq b c d)))))
    = (0 0)

    | (define literal (macro (s a e) (head a)))
    | (define bind (macro (self args env)
    |   (eval
    |     (prepend (prepend (head args) (prepend (eval env (head (tail args))) ())) env)
    |     (head (tail (tail args))))))
    | (display
    |   (bind qqq
    |     (macro (self args env)
    |       (if (equal? args ())
    |         (eval env (literal (prepend b (prepend d ()))))
    |         (eval (prepend (prepend (head args) (prepend 0 ())) env)
    |           (prepend self (tail args)))))
    |     (bind b 1 (bind d 4 (qqq x y z)))))
    = (1 4)

Your recursive `macro` application doesn't have to be tail-recursive.

    | (define literal (macro (s a e) (head a)))
    | (define bind (macro (self args env)
    |   (eval
    |     (prepend (prepend (head args) (prepend (eval env (head (tail args))) ())) env)
    |     (head (tail (tail args))))))
    | (display
    |   (bind make-env
    |     (macro (self args env)
    |       (if (equal? args ())
    |         ()
    |         (prepend (prepend (head args)
    |                     (prepend (eval env (head args)) ()))
    |           (eval env
    |             (prepend self (tail args))))))
    |     (bind b 1 (bind d 4 (make-env b d macro)))))
    = ((b 1) (d 4) (macro macro))

`macro` expects exactly two arguments.

`macro` expects its first argument to be a list of exactly three
symbols.

### `raise` ###

`raise` evaluates its argument to obtain a value, then raises an
exception with that value.

If no exception handlers have been installed in the execution
history, the Robin program will terminate with an error, ceasing execution
of all Robin processes immediately, returning control to the operating
system.  For the sake of usability, the error should include a message which
refers to the exception that triggered it, but this is not a strict
requirement.

    | (display
    |   (raise 999999))
    ? uncaught exception: 999999

`raise`'s single argument may be any kind of value, but `raise` expects
exactly one argument.

A Robin environment may install exception handlers which are not defined
in Robin code itself.  (i.e. exiting to the operating system is not a
strict requirement.)

### `catch` ###

`catch` installs an exception handler.

If an exception is raised when evaluating the final argument of
`catch`, the exception value is bound to the symbol given as the
first argument of `catch`, and the second argument of `catch` is
evaluated in that new environment.

    | (define literal (macro (s a e) (head a)))
    | (define list (macro (self args env)
    |   (if (equal? args ())
    |     ()
    |     (prepend (eval env (head args))
    |               (eval env (prepend self (tail args)))))))
    | (display
    |   (catch error (list error #f)
    |     (raise (literal (nasty-value 999999)))))
    = ((nasty-value 999999) #f)

`catch` *cannot necessarily* catch exceptions raised by intrinsics.
It ought to be able to catch exceptions raised by intrinsics wrappers,
though.

The innermost `catch` will catch the exception.

    | (define literal (macro (s a e) (head a)))
    | (define list (macro (self args env)
    |   (if (equal? args ())
    |     ()
    |     (prepend (eval env (head args))
    |               (eval env (prepend self (tail args)))))))
    | (display
    |   (catch error (list error 5)
    |     (catch error (list error 9)
    |       (raise (literal derpy-value)))))
    = (derpy-value 9)

An exception raised from within an exception handler is
caught by the next innermost exception handler.

    | (define list (macro (self args env)
    |   (if (equal? args ())
    |     ()
    |     (prepend (eval env (head args))
    |               (eval env (prepend self (tail args)))))))
    | (display
    |   (catch error (list error 5)
    |     (catch error (list error 9)
    |       (catch error (raise (list error error))
    |         (raise 7)))))
    = ((7 7) 9)

`catch` expects its first argument to be an identifier.

`catch` expects exactly three arguments.

TODO we should probably have some tests that prove that `catch` can
catch errors raised from inside macros.
