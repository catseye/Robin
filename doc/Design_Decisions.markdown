Design Decisions
================

The design space for programming languages is monstrously large.  When you
are designing an esolang, you have the luxury of narrowing down the design
space, essentially arbitrarily, and focusing on a handful of computational
gimmicks, and how they interact.

In the process of designing a more "real" language, however, you have no
such luxury -- ideally, all of your choices should have reasons behind them.
There are no "right" decisions, of course, but the choices should be
justifiable, given some set of goals.

Perhaps more importantly, all of the reasons should be coherent, when taken
together -- the same justifications should support all of them.  There is no
point in justifying one decision with "it should be simple to implement
instead of simple to program in", and another with just the opposite -- the
end result will be a hodge-podge, and we might as well have just made our
choices arbitrarily, without any justification at all.

Having had to make the design decisions behind Robin, I will try to document
the major ones here, and the reasons behind them.  Of course, since Robin's
design is still under development, many of these are subject to change.

Meta-Design
-----------

#### Should Robin be rigorously specified?

Decision: Absolutely.

A rigorous specification of a language allows two things:

* Proofs of properties of programs in the language.  Without a formal
  semantics, this just isn't possible.  It should also be standardized
  (that is, there should be an "official" definition): it's all well and
  good to independently define "a formal semantics" for some programming
  language, but if different programmers are using different formal
  semantics for the same language, they can't exchange their proofs.

* Commodification of implementations of the language.  Allowing
  implementors to independently implement the same language leads to a
  "marketplace" of implementations, which (under prevailing economic
  theories, anyway) leads to higher quality implementations through
  competition.

#### Should Robin's core language have a simple definition?

Decision: Yes.

Keeping the definition simple contributes to the same commodification goal
listed above: it lowers the barriers to implementation.

Providing a lot of useful things in the core does make some things handier
for the programmer, but it does increase the effort to implement the
langage (think of all the nooks and crannies of Perl.)  Instead, all these
handy things should be packages in modules, which need not always be
imported or used.

Approaching this naively can lead to inefficiencies, however, as more
advanced functionalities must be built up from simpler functionalities,
and the "sufficiently clever compiler" that can optimize these is hard to
come by.  So, if there are any measures we can take to mitigate this
effect, without destroying simplicity -- we should investigate them.

#### Should Robin be defined with denotational semantics?

Decision: No.

It's inaccessible to most programmers, and it is essentially just
another programming language, which is itself not perfectly well
standardized.

A much better choice is the programming language Haskell.  It is
quite well defined, quite close to denotational semantics (in its
pure form, anyway), and above all, executable -- leading immediately
to a usable reference interpreter.

#### Should Robin be defined using multiple definition languages?

Decision: Yes.

The method of description should employ at least two descriptions in two
language-describing languages.  This way, a form of "error-detecting
code" applies: each description can be checked for consistency against the
other.  (Using three languages would permit a form of "error-correcting
code": whichever behavior is in at least two of the descriptions is
considered official, and the third is considered erroneous.  But this is
possibly too burdensome in practice.)

Given Haskell as one of the definition lanaguges, the logical choice here
is Literate Haskell, with each part of the Haskell definition accompanied
by a definition in (relatively formal) English.

#### Should the language also be defined with conformancy tests?

Decision: Yes.

Of course, it's very difficult to compose tests which actually define a
language.  You can't effectively test that such-and-such a program leads
to an infinite loop, and you can't effectively test that such-and-such a
program has the same behaviour on *any* of an infinite possible set of
inputs.

But, you can write tests that detect a finite number of points where an
erroneous implementation fails to meet the definition.  And, you can
execute these tests on a computer -- in the process of developing a new
implementation, this can help a lot.  And, this brings the definition
closer to being "in triplicate" and thus having some properties of an
error-corrcting code.  So, conformancy tests should definitely be part of
the language's documentation.

Design Proper
-------------

#### Should Robin's syntax be based on S-expressions?

Decision: Yes -- but it should not be the *only* syntax.

Robin, as it stands currently, is a "sugar-free" language.  Programs and
modules are represented concretely as S-expressions, which typically map
directly to the AST (abstract syntax tree) used by the implementation.

Research in linguistics suggests there is such a thing as too much
regularity in a language for human comfort.  All spoken languages have
some irregularity in them.  When constructed languages such as Esperanto
are taught as native languages to children, they tend to be "irregularized"
as they are acquired.  Perhaps the human mind needs these irregularities as
"handles" to better grasp the ways to express concepts, or perhaps it uses
them as "checksums" for error correction and disambiguation -- but these
are just pet theories.  Whatever the reason is, it happens.

My point is, S-expression-based languages are certainly a formal instance
of language structure which is "too regular for comfort", so programming
in Robin (or Scheme, or Lisp) often tends to be somewhat brutal (especially
without editor support to match parentheses for you.)

However, mathematically and in software engineering, this regularity
provides immense benefits, because it both makes the structure of
the language simple, and thus easy to define and analyze, and makes the
language very expressive -- the ease of writing code that works on code
makes it possible to create very flexible and coherent (I daresay
"powerful") abstractions.  So, Robin errs on the side of this benefit.

However, there is no reason that Robin should fixate on this syntax.
It is important not to neglect usability, and, although one has not yet
been devised, there is no reason that Robin cannot have other, more "humane"
alternate syntaxes which are easier to read and write.

A sugared "humane" syntax might look like the following.

    robin 1.0
    import small 1.0

    pi = 3.14159

    fac(x) =
        if x <= 1 then 1 else
            r = fac(x - 1)
            r * x
        end

    fac(7) * pi

It would be translated by a pre-processing step to something like:

    (robin (1 . 0) (small (1 . 0))
      (bind pi 314159/100000
        (bind fac (lambda (self X)
          (if (<= x 1)
            1
            (bind r (self self (- x 1)) (* r x))))
          (* (fac fac 7) pi))))

#### What should be in the core?

Decision: A semantically minimal set of macros.

I went back and forth before deciding what should be in the core and
why.  One possibility was to make it the same as Pixley.  But macros
would be added to it, and macros would need to be in the core (as they
can't be written directly in Pixley), and once you have macros, a lot
of the Pixley functions, like `let*` and `cond`, *can* be written in
the language.  So, should they remain in the core?

I decided no: the core would be simpler to implement and analyze
without them.

The only place where I waver on this currently is `fun`.  While `fun`
*can* be defined as a macro, it is so basic to writing modules in
Robin, that it is very tempting to place it in the core.  (The version
defined as a macro is very inefficient, but of course the `small`
module need not be implemented in Robin itself.)

#### Should all programs be contained in some kind of header form?

Decision: Yes.

We want to be able to quickly identify what S-expressions are
Robin programs, and what aren't, especially if we're using some of the
same identifiers as other languages, like Scheme.  Also, this is a
good place to specify the version of Robin in use, and a good place
to import required modules.

An alternative idea was some kind of meta-format called "Parts":

    (parts (import (robin 1 0) ...)

But "Parts" would not establish the deep semantics of the language
(reduction order, etc.)  And subsequent imports might rely (heavily)
on those semantics.  Meaning, imports would have to import fundamental
semantics, and imports would depend on that being imported first,
and, the result is just ugly.

#### Should you have to import the core?

Decision: Yes.

This is actually a special case of a more general design decision,
namely:

#### Should modules be fine-grained?

Decision: Yes.

If modules are fine-grained, and only a few are truly required, the task
of implementing (or porting) the language is much simpler.

This applies as well to architectures that don't support all functions
in all modules.  For example, clockless systems won't have a way to
retrieve the current time of day.  *But*, such systems would still be
capable of manipulate date and time values.  Therefore, those two sets
of functions, though closely related, should not be bundled into the
same module.

It's true that it's annoying for the programmer to remember which
module a function is in.  For this reason, we can have "umbrella modules"
which simply re-export all the functions in a large set of standard
modules -- assuming there are no name conflicts amongst them.

More philosophically: if something is part of the core semantics of
the language (like error codes,) should it be put in a module?  Largely
I've been able to arrange things to avoid this issue.  For example, if
`head` fails, it raises an exception if the implementation supports
exceptions, otherwise it just aborts execution.  But, when when support
for exceptions exists, if a raised exception is not caught, execution
is aborted -- so the behaviour is compatible.  However, there are
potentially other instances of "semantics for this are in the core, but
you have to import this module to get at thim" -- I've seen them in
other languages, and when I remember or re-find an example of it, I'll
add it here.

#### Should importing be done in the header, or by a function?

Decision: In the header.  
Chance of changing: Non-zero.

Importing modules in the header is a form of statically declaring the
dependencies of a program; if one of the modules isn't available on
some system, it can instantly say "no, I can't run this."

If there was instead a function to import modules, such a system would
need to statically analyze the program to see if dependencies are met
(see Python's `setuptools`).  When it can't figure that out exactly,
which is inevitable, the program will break at some arbitrary point
during execution.

Also, importing via a function would require that the function to do
the importing would be exported before everything else; in other words,
`(robin (1 0) ...)` would need to export one function, `import`.  This
is slightly un-orthogonal.

The downside of statically declaring the modules in the header is that
you might want to write a program which is somewhat flexible: if a
particular module is available, it will take advantage of it, but if not,
it will fall back to something perhaps less optimal but still usable.
You can't do that in the current regime.

However, there may be better ways to think about this, and they go back
to ideas I had about Robin when it was in my mind more like an operating
system.  The issue is often not the availability of a module but rather
the availability of a resource; modules are, at best, definitions,
rather than suppliers, of resources.  But, I will have to think about
this more.

#### Should function names follow in the Lisp/Scheme tradition?

Decision: No.

It's good to have roots, but there are limits.

Lisp/Scheme names posess a lot of awfulness due to their legacy.
`cdr` absolutely sucks as a name.  Unfortunately, things like `tail`
and `snd` aren't standard replacements for it, yet.  `lambda` is
less offensive, but only because it's a widespread standard; there is
nothing except Church's work that ties the Greek letter lambda to
the idea of a function, and even that is, if you believe the folklore,
mainly due to typesetting limitations he encountered in publishing.

If the programmer really wants Lisp/Scheme names, they can always
define them in a "compatibility module".  (In fact, I should probably
anticipate this, and accomodate it with an established convention.)

#### Should `#t` and `#f` be Church booleans?

Decision: No.

While it's tempting in that it would allow us to not have `if` in the
core, it just moves that complexity from `if`, a built-in macro, to
the evaluator and/or type system.  Having an explicit, separate `if`
lets `#t` and `#f` be more like plain symbols.  In fact, one day, they
might be classified as such -- if I can grapple other design decisions
in the way of that.

#### Should Robin allow improper lists?

Decision: Yes
Chance of changing: Moderate.

Drawing directly from the Lisp/Scheme tradition, and being supported by the
idea that the core semantics should admit as much "goo" as possible ("it's
not a language so much as it's a building material"), with static analysis,
if desired, being layered on top of that -- improper lists are currently
allowed in Robin.

However, there are several points not in their favour, and I might remove
them.

* We may want to base everything on "goo", but we should want clean "goo".

* You can always simulate an improper list with a proper list with some
  kind of marker term at the end.

* The very name "improper" should be a big hint that these constructs are
  not clean.  (However, this argument could be regarded as sophistry.)

* Various functions in the `list` module currently have slightly different
  behaviour on proper versus improper lists.  Proper lists only would make
  them more orthogonal.

* Improper lists maybe have a place in history; when resources like memory
  were scarce, they were a way of saving a cons cell.  However, this now
  goes against treating resources as not scarce in order to have a more
  abstract and elegant description of programs.

* When you have both proper and improper lists, `list?` is O(n); with only
  proper lists, `list?` is O(1), basically the same as `pair? or null?`.

#### Should we require lists in syntax where they aren't strictly necessary?

Decision: Sometimes yes, sometimes no.
Chance of changing: High.

This wasn't really a conscious decision, so much as something that should
probably be cleaned up.

By example: Scheme's `let*` requires that you put all the bindings in a list:

    (let* ((a 1)
           (b 2)
           (c 3))
       (foo a b c))

That intermediate list isn't really necessary; the implementation of `let*`
could just treat the last term as the expression to be evaluated in the new
environment:

    (let* (a 1)
          (b 2)
          (c 3)
      (foo a b c))

This is good under the theory "the fewer parentheses, the better", and this
is not a bad theory.  Also, it is perhaps less efficient (because the
implementation must look ahead to see if something is a binding or not), but
again, resources should not be considered scarce; it can always be converted
internally to something more efficient.

But, Robin will one day have a more humane syntax, so that programmers won't
have to deal with these forms unless they want to.  The intermediate list
could also be seen as more orthogonal to the semantics (you really are
working with a list of bindings, and you shouldn't overload the meanings of
things in the list.)

So: in the `(robin (0 1) ...)` form, there is no seperate list of module
imports; but Robin's `let*` does have an intermediate list.  (On the other
hand, `bind` doesn't need a list at all, obviating the issue.)  Generally,
there is no consistency here yet, and one should probably be established.
