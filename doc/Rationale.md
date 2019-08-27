Robin: Design Goals and Rationale
=================================

This document documents some of the design goals and rationale
for the design of Robin 0.3.  The contents are not well organized,
and the document is not very comprehensive.

In this document, "Robin" refers to the Robin programming language
version 0.3.

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

    <p><i>(env)</i> env? export sandbox unbind unshadow</p>

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