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

#### Should Robin be a general-purpose language?

Decision: Yes.

Not much to say about this.  A general-purpose language with a module
system, not to mention macros, can be customized for specific purposes.
(We need to do more investigation into how "teleological contexts" and
metadata on values can be leveraged for such purposes.)

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

Just because programmers are familiar with a notation or concept is not
enough of a reason to incorporate it into the language's foundation. At
the same time, we'd obviously prefer not to alienate programmers
completely (that's what esolangs are for!)

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

Decision: No.

Drawing directly from the Lisp/Scheme tradition, and being supported by the
idea that the core semantics should admit as much "goo" as possible ("it's
not a language so much as it's a building material"), with static analysis,
if desired, being layered on top of that, improper lists were originally
allowed in Robin.

However, there are several points that can be made against them, so they
were removed from the language.

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

Decision: Yes.

What do I even mean by this?  Well, for example, Scheme's `let*` requires
that you put all the bindings in a list:

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

So, Robin's `let` does have an intermediate list.  (On the other hand,
`bind` doesn't need a list at all, obviating the issue.)  Following suit,
the syntax for importing modules uses a list to contain the module specifiers
(although it did not originally.)

As a corollary to this, `choose` should probably have a list of conditions,
and should not need an `else` branch -- the "body" of the `choose` should
be the "else".

#### Should the language define static analyses?

Decision: No, but it should accomodate them.

This is a pretty subtle issue, which is explained more fully in the
Static Analysis document.  But in short, to work towards the goal of
keeping the language simple, we want to move things out of it, to modules
and/or to implementation issues (such as configuration files), and one
of the things we can move out is static analysis.

At the same time, the language should be designed to accomodate static
analyzers that are built on top of it, and some of those static analyzers
should be standard.

A language can define a type system without specifying that types should
be checked statically.  However, if no thought is put into how easily the
types of a program can be statically analyzed, this raises barriers to
actually doing it.  Static analyzers in the world of scripting languages
in particular are often an afterthought, and we want to try to minimize
that effect.

#### How should serializability be handled?

Decision: ...

OK, so: one reason to choose an S-expression based syntax is that terms
so represented are, for the most part, "trivially serializable", because
many forms simply evaluate to themselves, and when they don't, they can
be wrapped in `literal`.

This is useful because, where-ever values are serialized (disk files,
messages between nodes, etc.,) they look just like they would in a program
text.

However, there is an inherent tension between concrete representations and
abstract types.

In the following, _erroneous access_ means transformations of data that
result in a non-conformant structure.  _Interchangeability_ means only
allowing a set of operations whose implementations can be changed.

Concrete representations serialize trivially, and can be pattern-matched,
but they do not prevent erroneous access, nor support interchangeability.

Abstract types prevent erroneous access and support interchangeability,
but they do not serialize trivially, nor can they be pattern-matched.

(Abstract types are also traditionally desirable because they allow
optimizations, but since performance is a non-goal of Robin, we won't
discuss that here.)

A solution is to ensure every abstract type has two operations, `to-repr`
and `from-repr`, which convert the abstract value into a concrete
representation and vice-versa.  `to-repr` should be deterministic; for all
values _v_ of some abstract type _t_, all implementations of _t_ should
produce the same value for `to-repr` _v_.  For example, an abstract type
of dictionaries might have as its representation a sorted, non-redundant
alist.

This permits serialization, pattern-matching, equality testing, etc.,
simply by (implicitly) calling `to-repr` on the value first.

These two functions should be round-trippable, in that for all _v_,
`(from-repr (to-repr v))` = v.  Some information may be lost, but
such information should not be critical information (e.g. caching most
recently used values for the sake of performance, etc.)

You can still try to perform erroneous access by converting the abstract
value to a concrete representation, mucking with the representation, then
converting it back to the abstract value.  However, the representation
should be defined at the level of properties of the abstraction, and trying
to convert it to an abstract value should raise an exception if those
properties do not hold (i.e. if the concrete value is "corrupt".)

We could treat macro values as abstract values.  The representation of a
macro value is its definition, plus any values that might be closed over in
it (represented as a `let` wrapping the definition.)  But there is one
desired property that does not hold -- the representation of a macro is not
deterministic; there are an infinite number of equivalent representations of
any macro, and no effective procedure to select the "simplest" one (or any
other way to effectively order possible representations.)

We could treat pids as abstract values.  One might hope to do information
hiding with pids; you should be unable to send a message to a process unless
you got its pid from it or its parent (perhaps indirectly).  However,
`from-repr` lets you make up pids "from scratch" (from arbitrary concrete
representations) and even if the pid structures contains an obscure
fingerprint or the like, you might accidentally hit upon an existing pid.

However, maybe that is not so bad; we could call it the "how did you get
this phone number" problem.  Even if pids are abstract, a process can't
really rely on its parent not somehow sharing its pid with some process it
knows nothing about, barring some really involved proofs.  And, even with an
abstract pid, you can't guarantee e.g. that sending it a message has some
meaning; the process might have died, maybe long enough ago that the pid was
recycled and a new process lives there now.  (Though, of course, we should
take measures to reduce the chances of all these things.)

Can we implement abstract values as functions?  Take a queue for instance:

    (bind q (queue:empty)
      (((q enqueue 33) enqueue 66) deqpeek))

...should evaluate to 33.  This is pretty good.  What would the
implementation look like?

    (bind empty
      (bind contents ()
        (macro (self args env)
          ; if args#0 is 'enqueue', return a macro like 'self'
          ; closed over (pair args#1 contents)
          ; else if args#0 is 'deqpeek', return (list:last contents)
        )

But it looks like this might involve mucking with the closed-over
environment of the macro -- which we could make possible.  But I'm not
sure we want to.  Anyway, this macro would also need to implement the
operations `(q to-repr)` and `(q from-repr (list 1 2 3))`.  The latter
is actually a "class method" and doesn't need to be on an existing
instance; but that is its own, fairly involved design decision.

Also, Robin potentially has the ability to use different implementations
(representations) of the same abstract data type in different modules,
independent of the program: the module configuration file could point
one module to an alist-backed dictionary data type, and another module
at a hash-table-backed dictionary data type.  Those two modules ought to
still be able to pass objects of the dictionary data type back and forth
to each other.

#### Should environments be abstract data types?

Decision: Currently they aren't, but they should be.

Currently, all macros in the standard modules accept and return alist
representations of environments.  But there are mismatches between this
representation, and what environments actually support.  There can be
multiple entries for the same key in an alist, and alists expose an order
to their entries, neither of which is a necessity of environments.  There
are potentially many ways to represent an environment.  So, environments
should be encapsulated in an abstract data type that supports the
operations `lookup` and `extend`.

#### Should strings be abstract data types?

Decision: Again, currently they aren't, but they should be.

Again, there are multiple possible ways to represent a string: a naive
list of integers (representing characters) suffices for many applications,
but other applications may benefit from using a "rope" representation, or
some other representation.

#### Should there be a distinction between processes and devices?

Decision: No.

I backtracked on this one.

It's tempting to unify the two, and say that there are only devices,
and that every concurrent process you spawn is a device of some sort.

The argument against unifying the two is that devices represent resources
beyond just cycles and memory, while you can have a "compute" process
which just uses cycles and memory.  It doesn't need to be acquired, or
released, or registered for access by some other, anonymous program.

But that's not quite true.  It's not "just cycles and memory"; cycles
and memory, and the privilege to create a process that uses them,
constitute a device that must be acquired (started), released (stopped),
and possibly even registered for access, if the notion of IPC involves
sending messages from process to process.

So I think we need a concept of a "processing device".  Normally this would
be a "virtual processor" (backed by an OS process or other simulated
concurrency.)

But also -- other devices may or may not acquire their own virtual processor,
or use an existing virtual processor instead.  These two options map to
having each service in its own process (an Erlang ideal) and to having a
library of functions that run "inline" (in the caller's thread -- a C reality.)

It would be great if devices could be contrived so as to be flexible on that
point.

Starting up a virtual processor to run a Robin program is a bootstrapping
issue.  You generally wouldn't have to write any code to acquire the initial
processor device -- I mean you couldn't: what started the virtual processor
that *that* code was running on?  This would instead be the responsibility of
Robin's kernel, which is, conceptually, a black box in this regard.  But you
could start *another* processor device, in your code, to run your code --
basically, to spawn a process.

See also the "Programming Languages vs. Operating Systems" section I just
added to the Practical Matters doc (though it doesn't really belong there.)

#### Should the `random` facility be a device?

Decision: No.

Following the decision immediately above, and the decision to have
fine-grained modules -- the `random` module itself is simply doing
computation (generating ever-more pseudo-random numbers from a seed.)
*However*, it may be seeded with a source of entropy from the system --
which implies that there should, indeed, be an `entropy` device.

But considering naming conventions -- possibly, for the same reason,
the `random` module should be called `pseudo-random` or similar.  And
`entropy` might likewise be better named `random`.  Not sure.

#### Should all messaging transactions be synchronous?

Decision: It's tempting, but I'm starting to think it's not practical.

While the Erlang paradigm for message-passing is very simple -- just
`Pid ! msg` and you've sent a message -- it's also very low-level.  For
Erlang/OTP, things like `gen_server` are built on top of this, using
sets of functions to abstract away the details.  Robin does something
similar with `call!` and `respond!`.  This lets you write code where you
can be reasonably sure that the other processes you are sending messages
to are in the state that you expect.

Being able to send a message to a process and not expect a reply does
let you write potentially more efficient code; you don't have to wait for
your message to be acknowledged.  But combining this with `call!`/`respond!`
can lead to complex patterns of communication, where it is difficult to
reason about whether the other processes are in the state you expect.
(If the server process uses `respond!`, what should it do about messages
that don't require an acknowledgement?  How sophisticated does `respond!`
need to be?)

Also, requiring synchronous communication between processes does not
preclude asynchonous processing -- it's just that your "start doing this"
message needs to be acknowledged by the other process, and your process
needs to wait for that acknowledgement.  The activity itself still goes
on independent of the current process.

Also, race conditions are among the hardest bugs to detect, isolate and
fix, and unacknowledged asynchronous messages probably lead to them (though
not so much as shared updatable storage leads to them.)  Doing what we can
to encourage programmers to avoid race conditions in design is probably
called for.

However... the problem is that if all messaging is synchronous, you lose
one of the main benefits of messaging, which is (this sounds tautological)
asynchronicity.  If you always need to wait for another process to confirm
that it got your message, you can't do anything else in the meantime.

Perhaps there is a way around this.  I'll need to come up with some
examples and write them down here.

#### Should all messaging consist of message structures (with envelopes)?

Decision: Again, not sure, leaning towards yes.

Again, `Pid ! Msg` where `Msg` is anything is very simple, but again, it
is very low-level.  It is useful to have metadata about the message, like
which process sent it, and when.  But I need to think about this more.

#### Should symbols be atomic?

Decision: Currently, yes.  
Chance of changing: High.

In the Lisp tradition, symbols are "atomic" -- you can't take them apart,
you can't (always) create new ones at runtime, they have no intrinsic order,
and so forth.  Beyond a certain conceptual cleanliness, this has the
advantage that each symbol can be associated with, and after a compilation
process, can be replaced by, a small integer of fixed size.  This is good
for efficiency, but efficiency is not what we're going for here.  Later
languages in the Lisp tradition introduced things like `gensym` to create
new symbols, and a separate string type and operations like `symbol->string`
and `string->symbol`.

Being able to create new symbols during runtime is very useful in program
transformation, and for our purposes, should outweigh any efficiency gains
from having a fixed set of atomic symbols.

Also, if two nodes of a distributed Robin system are exchanging terms
produced by software the other knows-not-what, they *must* be prepared to see
symbols that they don't already know.

In Robin, we can regard having a fixed, static set of symbols in use in a
program as a *luxury*; if a static analysis reveals that you can "afford" it,
(i.e. that you're not creating new ones at runtime and so forth,) a compiler
can replace the symbols with small integers for efficiency in your case.

### Should symbols and strings be different types?

Decision: Currently, yes.  
Chance of changing: High.

Given Scheme's approach to the above (`symbol->string` and `string->symbol`),
let's ask ourselves: what's *really* the difference between the two types?
In an essentially purely functional language like Robin, both symbols and
strings are immutable.  In implementations where strings are "intern'ed",
this is not so different from inserting symbols in a symbol table.  And,
in a language like Robin where equality and identity are identical,
`(equal? "foo" "foo")` should evaluate to true whether the two strings are
two copies of the same text, or two pointers to the same single text.

This all points to the idea that symbols and strings should be unified; or
rather, that there should be no symbols, only strings.  A traditional
symbol literal, then, should be one syntax for specifying a string; and on
the other hand, you ought to be able to say things like

    (''let'' ((''( ('' 1)) ('X'+'X' 1 ''( (''))

with impunity.

### Should macro-type values even exist?

Decision: Currently, yes.  
Chance of changing: non-zero.

PicoLisp has some good ideas here (although identifying numbers with
machine addresses of functions strikes me as going way too far.)  Robin
has already wholeheartedly adopted the macro (which treats its arguments
as unevaluated terms) as a core abstraction, and builds functions on top
of them.  However, it could get even closer to PicoLisp's paradigm here,
and not treat macros as their own data type.  Instead, they are simply
terms of a certain form which, when they appear as the first element of
a list being evaluated, are expanded through substitution.

One reason to do this is that serializing, or otherwise depicting, a
macro is somewhat problematic.  We can use the definition that was used
to define the macro, but it's awkward.

Here's an example of what would be different if there were no macro-type
values.  Take the following code:

    (bind dup (macro (self args env)
                (bind a (eval env (head args))
                  (list a a)))
      (dup (literal foo)))

Currently, this would be evaluated as follows.  The `(macro ...)` term
evaluates to a value of macro type, and this value is bound to the name
`dup` in a new environment.  The `(dup ...)` term is evaluated, `dup` is
looked up in the environment to find that value of macro type, and that
macro definition is evaluated with the given arguments.

Here is what would happen if macros were just terms.  First, the code
would need to be written something more like:

    (bind dup (literal (macro (self args env)
                         (bind a (eval env (head args))
                           (list a a))))
      (dup (literal foo)))

Then, during evaluation, `dup` is bound to a literal term in a new
environment.  The `(dup ...)` term is evaluated, and `dup` is looked up
in the environment to find a literal term.  That term is virtually
inserted in the term being evaluated:

    ((macro (self args env)
       (bind a (eval env (head args))
         (list a a))) (literal foo))

This term is examined, and it is found to conform to a "macro evaluation
form"; it is virtually expanded thusly:

    (bind self (macro (self args env)
                 (bind a (eval env (head args))
                   (list a a)))
      (bind args (literal (literal foo))
        (bind env (env)
          (bind a (eval env (head args))
           (list a a)))))

Although the `(env)` may be fudging it a bit, this evaluation process is
now rather nicely depictable in Robin (although an implementation would
almost certainly make the new environment itself, without evaluating `bind`
to do so.)

This still doesn't help much with dealing with the same issues surrounding
built-in macros, though.
