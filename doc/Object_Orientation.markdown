Object Orientation
==================

Should Robin support object-oriented programming, and if so, in what sense
should it be "oriented" towards "objects"?  It's a tough question.  It is
so knotty, in fact, that instead of writing about in the Design Decisions
document, I'm going to write about it in this standalone document -- even
if Robin's support for it turns out to be minimal or non-existent.

Why would we want object-orientation?  To achive polymorphism at the value
level.  We already have polymorphism at the module level (your configuration
file can point a module at any given implementation of it) and the device
level (the device manager can return any device it likes for your request,
so long as that device supports the requested operations).

Still, polymorphism at the value level would be handy.  Consider
environments.  We don't care how they're represented, so long as they support
a few operations -- `lookup`, `extend`, perhaps `to-alist` and `from-alist`
or the like.

It doesn't make sense to model environments as devices, because
they have no independent existence (through time, or otherwise) from the
code that uses them.

We could simply provide multiple implementations of an `env` module, which
use different representations, but all support the given operations.
However, this does not buy us any data hiding.

So let's consider what an object system in Robin could look like, and how
it could be implemented.

Approach
--------

I would like to see the style of object-orientation to be prototype-based
(with "manual" inheritance; if your object doesn't understand a method, it's
up to it to pass that request on to some other object) and duck-typing based
(no explicit classes; if an object supports some set of methods that a duck
supports, then it's a duck, or at least you can treat it like one.)

Should Robin be "purely" object-oriented -- that is, should *everything* be
an object, or should there be some things which are not objects?

I haven't decided.  If it is "pure", it will surely make the fundamental
semantics more complex, because the base types will need to support some
basic methods "out of the box".

But the reason the core should be simple is to make a rigorous definition
easier, and if we have a rigorous definition, and it's still not too hard to
make a rigorous definition, maybe the extra complexity is worth it.

If it's not purely object-oriented,

* There might be some values I can't treat as objects.  Oh noes.
* Different groups of developers might make up their own incompatible object
  systems (think: Lua.)

The second problem could be mitigated by providing an object system in the
standard library -- you don't *have* to use it, but its presence would
encourage developers to build their objects in a compatible way.  (Kind of
like Perl; there, it's a convention, but a pretty strong convention.)

Implementation
--------------

Objects, and their methods, can be implemented (almost) using macros.  An
object is a macro which expects its first argument to be a literal symbol
which is the method name.  The rest of the arguments are the argument to
the method.  The return value is a (possibly) modified version of the object,
or some other value, or possibly even a pair of these two things.

I say "almost" because I think we need dynamic binding to do it purely as a
macro, or at least it would be really, really helpful.  Consider:

    (bind val 0
      (macro (self args env)
        (let ((method (head args)) (args (tail args))
          (choose
            ((equal? method (literal inc))
              (bind val (+ 1 val) self))
            ((equal? method (literal dec))
              (bind val (- 1 val) self))
            ((equal? method (literal what))
              val)
            (else
              (super method args))))))

How would this look?
--------------------

For example, lists:

    (LIST head) -> VALUE
    (LIST tail) -> LIST
    (LIST cons VALUE) -> LIST

We don't have a `List` class (and we don't want one either.)  Luckily, we
have a built-in prototypical object for lists, `()`, so we can say...

    (() cons 5)

...but this actually makes the list `(5)`, i.e. it's in an unusual order:

    ((() cons 5) cons 7)

That one makes the list `(7 5)`.

Things have a Smalltalk-y flavour after doing this.

Of course, those are only the methods that lists implement.  A list,
ultimately, should conform to a collection interface that other collections
conform to, which supports things like `fold`, `find`, `size`, and whatnot.
Basic lists should probably not support these out-of-the-box (the core would
be too complicated.)  But, it should be possible, maybe, to wrap a list in a
list-container, like

    (list-container ((() cons 5) cons 7))

If Robin is purely object-oriented, where do the non-instance methods live?
Do we have class objects (metaclass etc a la Smalltalk?)  I'd rather not
(duck-prototyping only.)  So that suggests we either put all these methods
on some object (effectively a class object, in some aspects, perhaps), or
retain plain macros (if objects are built from them then they're still
available for that purpose), or something else...

Problems
--------

One of the biggest problems here is how objects would interact with static
analysis.  A macro which operates on an object doesn't care about how the
object implements the methods on it, but it *does* care that the method
operation meets the interface expectations.

To do static analysis on objects, we would need to codify the expectations on
the interface(s) that each object supports.  With traditional name-only duck
typing, this is not possible; we would need to extend names with the interface
module that they're from (so you can distinguish `tree:bark` from `dog:bark`.)

Also, we would need to start putting metadata into interfaces somehow.

We need to do this anyway, to some extent; if I make a new macro called
`assign` which works a lot like `bind` or `let` but has a different syntax,
how do I let the static analyzer know that it does that?  (For that matter,
how does the static analyzer know that the concurrency macros bind a new
value to one of the identifiers passed to them?)

So, this should probably be put on hold until I have a better idea of what
metadata will eventually look like.
