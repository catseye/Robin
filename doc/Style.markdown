Robin Style Guidelines
======================

Modules
-------

* Modules should be deterministic.  They should always export the same
  bindings (the same names bound to the same values) each time they
  are imported.

* Don't do input or output in a module.  (Logging is a different matter,
  or at least it will be, when Robin gets a `logging` module.)

Naming
------

* Module names should be singular: `list`, not `lists`.

* Don't smoosh names together; use hyphens to seperate individual
  words inside an identifier.  `get-args`, not `getargs`.

* Use established terminology from functional programming (Scheme,
  Haskell) when appropriate.

* Avoid atavisms like `car` and `cdr`.

* Avoid jargon unless there is no clearer option.

* Avoid bland, generic names unless your function is truly generic
  itself.  Examples: `data`, `index`, `pick`, `content`.

* Don't abbreviate unless the abbreviation is essentially unambiguous.

* Referential transparency: still working this out, and probably
  deserves a document to itself.  Robin isn't an imperative language,
  so the convention of ending an identifier with `!` doesn't really
  apply; but it can have side-effects.  Maybe:

  * No symbol at end: this function always evaluates to the same
    value, given the same arguments.

  * Ends with `>`: this function may send messages, but does not
    receive messages, so it always evaluates to the same value *and*
    sends the same set of outgoing messages, given the same arguments.

  * Ends with `<`: this function may receive messages, but does not
    send messages, so it always evaluates to the same value given the
    same arguments *and* the same set of incoming messages.

  * Ends with `!`: this function may send and receive messages.

  For the nonce, I'm going to go with `!` indicating that the macro
  may have side-effects.  However, this should ultimately be handled at
  a more semantic level.  Functions should be able to be defined as
  not having side-effects, and the function-definition macro should
  check this is the case (i.e. that the function never calls anything
  [which calls anything] which may have side-effects), and certain
  call sites should not allow functions to be called which may have
  side-effects.

* Multiple arguments: still working this out too.  When you have a
  commutative, associative binary operator, you often want to be able
  to apply it to more than two arguments.  There are actually three
  cases here:
  
  * Plain old binary operation: `(and a b)`.  This should exist,
    because it has the advantage of being of known arity, which could
    be of help to analyzers and sugared syntax.  It should implement
    short-circuiting, if that makes sense for the operation (not only
    booleans, but `(multiply 0 j)` need not evaluate `j`.)  The other
    cases below can reduce to repeated applications of this.

  * Fold of operation over list: `(and-list (list a b c))`.  This
    is not much more than sugar for the appropriate `fold`; in this
    case, `(fold and #t (list a b c))`.  However, it will evaluate
    all of the arguments and construct a list, which defeats short-
    circuiting.  Often the name of this can be based on a different
    English word (`conj` for `and`, `disj` for `or`, `sum` for `add`,
    `product` for `multiply`.)

  * Operation over mutilple arguments: `(and-many a b c)`.  This is
    semantically equivalent to `(and-list (list a b c))`, but this
    can support short-circuiting where the other cannot.

Conditionals
------------

* As much as possible, write `choose` structures where the branches
  are all _disjoint_: whenever one branch is true, all other branches
  must necessarily be false.  Also comment the conditions under which
  the `else` branch will be true.  (This style guideline is in lieu
  of a program analysis which can determine this for you.)
