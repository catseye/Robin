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

Syntax
------

Question: Should Robin's syntax be based on S-expressions?  
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

- - - - -

...more to come!...watch this space...
