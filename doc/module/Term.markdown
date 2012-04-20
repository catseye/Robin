Module `term`
=============

    -> Tests for functionality "Interpret Robin Program"

The `term` module exports macros and functions for working with
S-expressions as terms, that is, hierarchical trees of data.

### `subst` ###

`subst` evaluates all three of its arguments to obtain values of any type.  It
then returns a modification of the third value where all instances of the first
value (even those deeply nested within sublists) have been replaced with the
second value.

    | (robin (0 1) ((small (0 1) *) (term (0 1) *))
    |   (subst 4 5 4))
    = 5

    | (robin (0 1) ((small (0 1) *) (term (0 1) *))
    |   (subst 4 5 (literal (1 2 3 4 5))))
    = (1 2 3 5 5)

    | (robin (0 1) ((small (0 1) *) (term (0 1) *))
    |   (subst 4 5 (literal (one two three four five))))
    = (one two three four five)

    | (robin (0 1) ((small (0 1) *) (term (0 1) *))
    |   (subst 4 5 (literal (4 1 4 1 4))))
    = (5 1 5 1 5)

    | (robin (0 1) ((small (0 1) *) (term (0 1) *))
    |   (subst 4 5 (literal (1 4 (1 4 (4 1) 4) 4))))
    = (1 5 (1 5 (5 1) 5) 5)

    | (robin (0 1) ((small (0 1) *) (term (0 1) *))
    |   (subst 4 () (literal (1 2 3 4))))
    = (1 2 3 ())

    | (robin (0 1) ((small (0 1) *) (term (0 1) *))
    |   (subst (literal (turkey and cheese)) (literal (pastrami and rye))
    |     (prepend (literal (turkey and bacon)) (literal (turkey and cheese)))))
    = ((turkey and bacon) pastrami and rye)

### `subst-many` ###

    | (robin (0 1) ((small (0 1) *) (term (0 1) *))
    |   (subst-many (literal ((p 100) (q 200)))
    |     (literal (a d (r p q) q (p (z q) p p) p z q)))))
    = (a d (r 100 200) 200 (100 (z 200) 100 100) 100 z 200)

### `literal-with` ###

    | (robin (0 1) ((term (0 1)))
    |   (term:literal-with ((p 100) (q 200))
    |     (a d (r p q) q (p (z q) p p) p z q)))
    = (a d (r 100 200) 200 (100 (z 200) 100 100) 100 z 200)

Somewhat unlike `subst-many`, `literal-with` evaluates the expressions
given as values in the bindings.

    | (robin (0 1) ((core (0 1) *) (term (0 1) *))
    |   (literal-with ((p (subtract 200 100)) (q (subtract 350 150)))
    |     (a d (r p q) q (p (z q) p p) p z q)))
    = (a d (r 100 200) 200 (100 (z 200) 100 100) 100 z 200)

`literal-with` may be given an empty list of bindings; in this case it does
the same thing as `literal` would.

    | (robin (0 1) ((term (0 1)))
    |   (term:literal-with ()
    |     (a d (r p q) q (p (z q) p p) p z q)))
    = (a d (r p q) q (p (z q) p p) p z q)
