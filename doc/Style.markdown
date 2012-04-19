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

* Identifiers for macros which take their argument, analyze its
  structure, then either raise an exception if the structure is
  unacceptable, or evaluate it as an expression if it is acceptable,
  should end with `:`.  So we might have:

    (pure: (total: (fun (a b) (+ (list:length a) (list:length b)))))

  Unfortunately, this clashes with `:` as a module seperator.  Like,

    (foo:pure: (bar:total: ... ))

  So, we'll maybe switch to `.` as the module seperator, assuming that
  improper lists go away:

    (foo.pure: (bar.total:
      (fun (a b)
        (+ (list.length a) (list.length b)))))

Associative Binary Operators
----------------------------

When defining a macro which implements a binary operation which is
associative, write two versions:

* One version which takes any number of arguments.  The minimum number
  may be zero, one, or two, depending on the nature of the operation,
  but there should be no limit to the number of arguments; the
  operation can be applied successively to all of the arguments, two
  at a time.

* One version which takes a list, and applies the operation to all
  of the arguments in the list.  This is not much more than sugar for
  the appropriate `fold`.  Often the name of this can be based on a
  different English word than the binary operation would have (`conj`
  for `and`, `disj` for `or`, `sum` for `add`, `product` for `multiply`),
  but if no word is appropriate, suffix the symbol for the binary
  operation with a `*` character.

The first version may take advantage of short-circuiting, but the
second version cannot, by itself: it will evaluate all of the arguments
and construct a list first, which defeats short-circuiting.

A sufficiently clever analyzer can convert the first into successive
applications of a single two-argument version of the macro, and the
second into this form as well if all of the members of the list are
known at analysis time.

A purity analysis may be applied to check if this conversion can
happen.  The macros so defined should be decorated with metadata which
associates each with the other for the purposes of analysis and
conversion.  The details of this need to be worked out.

Conditionals
------------

* As much as possible, write `choose` structures where the branches
  are all _disjoint_: whenever one branch is true, all other branches
  must necessarily be false.  Also comment the conditions under which
  the `else` branch will be true.  (This style guideline is in lieu
  of a program analysis which can determine this for you.)
