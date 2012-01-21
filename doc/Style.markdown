Robin Style Guidelines
======================

Modules
-------

* Modules should be deterministic.  They should always export the same
  bindings (the same names bound to the same values) each time they
  are imported.

* Don't do input or output in a module.

Naming
------

* Module names should be singular: `list`, not `lists`.

* Don't smoosh names together; use hyphens to seperate individual
  words inside an identifier.  `get-args`, not `getargs`.

* Use established terminology from functional programming (Scheme,
  Haskell) when appropriate.

* Avoid atavisms like `car` and `cdr`.

* Avoid jargon unless there is no clearer option.

* Don't abbreviate unless the abbreviation is essentially unambiguous.

* Referential transparency: still working this out, and probably
  deserves a document to itself.  Robin isn't an imperative language,
  so the convention of ending an identifier with `!` doesn't really
  apply; but it can have side effects.  Maybe:

  * No symbol at end: this function always evaluates to the same
    value, given the same arguments.

  * Ends with `>`: this function may send messages, but does not
    receive messages, so it always evaluates to the same value *and*
    sends the same set of outgoing messages, given the same arguments.

  * Ends with `<`: this function may receive messages, but does not
    send messages, so it always evaluates to the same value given the
    same arguments *and* the same set of incoming messages.

  * Ends with `!`: this function may send and receive messages.
