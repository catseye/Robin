Robin: Design Goals and Rationale
=================================

This document documents some of the design goals and rationale
for the design of Robin 0.6.  The contents are not well organized,
and the document is not very comprehensive.

In this document, "Robin" refers to the Robin programming language
version 0.6.

Robin is excessively principled
-------------------------------

Many times I have encountered some badly designed corner of a
programming language or system and have said to myself, "No!
This is categorically wrong.  Programming languages should never
do this.  A correct design would..." et cetera, et cetera.

Robin is, in some sense, the result of recording various
instances of this and putting them together in a single
language.  (Plus some random stuff that I'm not sure how it
ended up in here.)

How well does the result cohere?  Not really.

It started off as an idea for how I would like to design an
operating system based on [Pixley][].  It is no longer an
operating system design, but the reactive portion of it is
still reminiscent of that.

The case for homoiconicity
--------------------------

Either a language is homoiconic or it isn't.

I often find homoiconic languages hard to read.  But having
thought about it at some point, I concluded that, if I had
to pick one of homoiconic or not-homoiconic as being
"better" in some absolute sense, I would have to pick
homoiconic.

Because suppose the language isn't homoiconic.  In this case,
it still has a syntax, and this syntax is canonical.
And the essence of the syntax can be described with an AST,
and it's likely the AST can be expressed in the language
itself.  Possibly even in the standard libraries of the language
there is a parser for the language that produces this AST.

Still, that AST is not canonical in the way that the
syntax is.  You can pick other ASTs that capture the syntax
equally well.

But if the language is homoiconic, then the language
defines both the syntax and the AST structure canonically.

Maybe you define a syntactic sugar on top of this AST.
This sugar is not canonical, but that is more appropriate
(somehow) than the AST being not canonical.

There may have been more to this argument than this, but
if so I've forgotten it at the moment.

The case for referential transparency
-------------------------------------

Very few languages actually forbid mutable data.  What they
do instead is provide something like `set!` but discourage
it.

But unless you actually forbid it, you leave the door
open for breaking referential transparency.  Even if you
write purely functional programs, there is always a doubt
when mixing them with other code: what if the function
that's being passed to my higher-order function is
destructively updating something somewhere?  This interferes
with reasoning about the code.  The statements you make
have to be under the assumption that no one is doing that.

Haskell is one of the few languages that gets this right.
Unfortunately Haskell is not homoiconic, and its type system
and lazy evaluation are oftentimes not things I'm looking
for, and provide a distraction to what I'm trying to say
with a piece of code.

(Also, Haskell is, as Simon Peyton Jones has remarked,
"the world's finest imperative language".  I'm looking
for the world's finest functional language though, right?)

Macro as fundamental abstraction
--------------------------------

This is certainly the most unorthodox feature, the one that departs
the most from Scheme et al.

It allows the language to have no "special forms" whatsoever.
(Scheme would need at least `define-syntax` if it wanted to define
`if`, `set!`, and the other parts of its syntax, as macros.)

Whether having no special forms whatsoever is advantageous in any
way, or not, remains to be seen.

One upshot is that any functionality expressible in the Robin
expression language, can be passed to a macro or a function, as
a parameter, or returned from a macro or function evaluation.

One also thinks it might make analysis of the code simpler — a
parser or analyzer doesn't need to account for any special forms.

But, in practice, since everything is a macro, `eval` is called a
lot, and `eval` poses a significant problem for analysis.

But also in practice, an analysis tool will expect that the "small"
library has been loaded, and that function calls will use `fun`
as defined there, and thus can base their analysis on the semantics
of that macro without caring about its definition, or that its
definition contains `eval`.

No variable numbers of parameters
---------------------------------

In a Scheme-like language, the list of parameters passed to a
function is itself naturally a list.  Robin in some
ways tries to deny this, rather than embracing it as Lisp
and Scheme do.

It does so in the name of correctness: it is incorrect
to pass more arguments to a function, than it expects,
so you should be informed of this by means of an exception.

But it goes further, with the doctrine that no function should
have a variable number of arguments.  If you want a function
to work on a variable number of values, you should pass that
function a list.

The reason for this is generally to make analysis easier.
This analsysi includes syntax (should it ever become relevant):
each function has constant number of parameters means the
parameters can be parsed deterministically, without
needing extra syntax such as parens to tell when they stop.

There is also the matter of generality.  Say a function
works on, not a single set of variable nuber of values,
but two sets of different kinds of data.  The natural
solution would be to pass it two lists.  Parsing the
arguments as a single list would allow or perhaps even
encourage passing both kinds of data in the argument
liker, perhaps with some kind of delimiter, but this is
a clumsy and stipulative solution which should be avoided.

By going this route, Robin does give up a certain kind of
simplicity.  Functions like `add` and `multiply` *can*
naturally be thought of as taking any number of parameters.
`compose`, a function to compose functions, would too.

But this is because these functions work on monoids.
And not all functions work on monoids (`divide`, for
example, is not associative.)  And when we do have
functions that work this way, we usually name them
differently: `sum` and `product` (and for functions it
could be `seq` or `pipe`.)  We can use these names for
the distinct versions of these functions that take lists,
and implement them with general monoidal processing
machinery a la `mconcat`.

Module System
-------------

Robin's module system is this: Robin does not have a module system.

We're still working this out, so bear with us.  Let's start with
some fundamental principles of Robin.  You may love them or think
they are stupid (I can't tell, myself,) but they are what they are.

*   The core Robin language includes only a handful of symbols,
    called _intrinsics_.  These represent functionality that would
    be impossible or highly impractical to write in Robin itself.

*   A Robin program may, of course, define new symbols internal
    to that program, by assigning them meanings in its environment.

*   The Robin language expresses Robin programs; it does not
    express metadata about Robin programs.

*   Corollary: the contents of a Robin program is kept separate
    from the metadata about that Robin program.

*   Corollary: a Robin program that uses a symbol which is defined
    outside of that program does not, and in fact _cannot_, care
    where it is defined.

*   Corollary: dependencies between Robin (sub)programs and/or
    modules is an implementation-level concern, not a
    language-level concern.

*   Corollary: how the reference implementation solves the problem
    of dependencies between Robin programs is not necessarily how
    any other implementation should solve the problem.

*   ... all the Robin language really "knows" is that a Robin
    program may be split up into seperate "files" (where "file" means
    "input of program text into the implementation", I guess.)

*   Robin recognizes a set of symbols, currently called `stdlib`,
    that (should) have a (relatively) fixed meaning in all Robin
    programs, whether they are used in any given program or not.

*   Note (that should be elsewhere?): most of the macros defined
    in `stdlib` are supposed to, intentionally, take a fixed number
    of arguments for some reason (nominally, to make some kind of
    future static analysis easier.)

*   It is something like Maslow's hierarchy of needs.  Robin's
    intrinsics make programming possible (*barely* possible —
    survival-level.)  Robin's `stdlib` makes programming liveable.
    If there was another level, it might make programming pleasant,
    even.

Some implications of this setup in practice are:

*   If you distribute a Robin program to someone else, you need to
    tell them (somehow) what other Robin (sub)programs/modules it
    depends on.

*   Actually this is hardly different from C, where dependency
    information is encoded both in `#include`'s and in a `Makefile`
    or similar, which links in the correct modules.  The difference
    in Robin is simply that there are no `#include`s.

*   Other languages, such as Haskell and Python, try to include
    all dependency information in the program source code itself.
    This does away with `Makefile`-type dependency information,
    but at the cost of entangling programs and metadata about
    programs into the same files, into the same language grammar.

*   It would be entirely possible to define a "Robin dependency
    language" which:
    
    *   describes the dependencies between different Robin programs
    *   informs a tool like `make`
    *   uses Robin's syntax
    *   and perhaps even embeds Robin as an embedded language
        (and thus perhaps appears as a Robin "top-level form")
    
    ...*but*, the important thing to note is that such a language
    would *not be Robin itself*.

*   Any symbol in `stdlib` could be implemented in any language
    whatsoever, as long as the implementation knows what the
    semantics of the symbol is.

*   To signal that a program requires some symbol to be defined
    before the program can be considered meaningful, it may
    assert that the symbol is defined, using the `assert`
    top-level form.

The more pragmatic aspect of how the reference implementation
currently handles the issue of dependencies between Robin programs,
keeping in mind that this is an implementation issue and _not_ a
language issue, and thus that the reference implementation is _not_
normative in this regard:

*   Each symbol defined in the Robin `stdlib` is written in its own
    Robin source file in the `stdlib` subdirectory, bundled along
    with tests for it.

*   All of the symbols in the `stdlib` directory are implemented in
    Robin.  This is because, being a reference implementation, they
    are "executable specifications" rather than production code.
    They are supposed to be correct and simple and understandable,
    rather than performant.

*   Groups of symbols in the `stdlib` are collected into files
    called "packages", in the `pkg` subdirectory, which are simply
    concatenations, topologically sorted by dependency, of those
    individual files in the `stdlib` subdirectory.  (These packages
    are built both by `./build.sh` and `./test.sh`.)

*   The groupings of symbols within a package follow certain themes,
    but are largely arbitrary, due to the ease with which a
    particular symbol could be grouped into two different packages
    by theme, and partly done for the convenience of the test suite,
    and to make dependencies work out "nicely", so that symbols can
    be implemented in terms of other symbols.

*   However, this package has the following justification:
    The package `small` is identified as a fairly minimal set
    of symbols to make programming tolerable
    (somewhere between possible and liveable in that "Maslow's
    hierarchy" analogy.)  No symbol in it depends on any symbol
    defined in any other package; only intrinsics and other symbols
    in `small`.  The functions in the `small` package have also
    been implemented directly in Haskell, in the reference interpreter.

Here is a graphical depiction of the "hierarchy" of defined symbols
(it's in HTML because it'd be trickier to depict in plain text or
Markdown.)

<table style="border: 1px solid; padding: 1em; margin: 1em">
  <tr><th>Standard Library</th></tr>
  <tr><td>

    <p><i>(boolean)</i> and or xor not boolean?</p>

    <p><i>(list)</i> empty? map fold reverse filter find append elem? length index
    take-while drop-while first rest last prefix? flatten</p>
    
    <p><i>(alist)</i> lookup extend delete</p>

    <p><i>(env)</i> env? bound? export sandbox unbind unshadow</p>

    <p><i>(arith)</i> abs add &gt; &gt;= &lt; &lt;= multiply divide remainder</p>

    <p><i>(misc)</i> itoa</p>

    <table style="border: 1px solid; padding: 1em; margin: 1em">
      <tr><th>"Small" Library</th></tr>
      <tr><td>
        literal
        list
        bind
        env
        let
        choose
        bind-args

        <table style="border: 1px solid; padding: 1em; margin: 1em">
          <tr><th>Intrinsics</th></tr>
          <tr><td>
            head
            tail
            prepend
            list?
            symbol?
            macro?
            number?
            equal?
            subtract
            sign
            macro
            eval
            if
            raise
            catch
          </td></tr>
        </table>

      </td></tr>
    </table>

  </td></tr>
</table>

[Pixley]: https://catseye.tc/node/Pixley
