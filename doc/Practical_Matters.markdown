Practical Matters
=================

This document is a collection of notes I've made over the years about the
practical matters of production programming languages -- usually stemming
from being irked by some existing programming language's lack of adequate
(in my opinion) support for them.  As such, these thoughts may be overblown
and sophistry-laden, and not even things I necessarily want to see in Robin.
But it is nice to have a central place to put them.

Fundamental Abstractions
------------------------

The following facilities should be either built-in to the language, or part
of the standard (highly standardized) libraries:

* Tracing.  Ideally, the programmer should be able to easily browse all the
  relevant reduction steps, and the relevant data being manipulated therein,
  in the part of the program's execution that interests them.  In addition,
  this should be something that can be enabled without polluting the source
  code (overmuch).
  
  This could be done, and fairly well, with techniques from aspect-oriented
  programming.  The rules to describe what to trace (or to highlight in a
  full trace) could be specified in what amounts to a configuration file,
  and thus be an implementation issue rather than a language issue.
  
  Unfortunately, this ideal is hard to achieve, so the system should also
  support...

* Logging.  Logging is basically an ad-hoc way to explicitly achieve
  selective tracing: the programmer knows what points in the program, and
  what data, are of interest to them, and outputs that data to the log at
  those points.

  Whether this is "debug logging" during development, or to support post-
  mortem analysis of issues in production, it amounts to the same thing:
  debugging, just on different time scales.
  
  The use of a "log level" is mostly just a way to filter the trace built
  up in the log files.  This is not necessarily a bad idea, but it should
  probably not be linear; information should be logged based on the reason
  that it is being logged, probably in the form of some sort of "tag", and
  filterable on that (whether at the time the log is being recorded, or
  being read.)
  
  In Robin, logging should not count as a side-effect.

  The logging function itself should have some properties:

  - Should not have side-effects (for example from evaluating its arguments),
    so that if it is not executed (because we are not interested in that
    part of the execution trace) the behaviour of the program is not changed.

  - In fact, should ensure that its arguments have no side-effects, and
    ideally, be total, with no chance of hanging or crashing.

  - Should pretty-print the relevant values, include the type and other
    metadata of the values, and put clearly visible delimeters around the
    values so printed.

  - Should include the source filename and line number.

  - Should not be overridable (shadowed?  not sure what I meant here.)

* History.  This is more relevant in a language with mutable values, but
  as part of tracing, it is useful to know the history of mutations of a
  value.  With immutable values, it would be useful to be able to view
  all the reductions which fed into the computation of the value at a
  point.  Either way, however, this is expensive, so should be specified
  selectively.  Again, an external, aspect-like configuration language
  for specifying which values to watch makes this an implementation issue.

* Command-line option parsing.  This should not rely on the Unix or DOS
  idea of a command line, and it should be unified with parameter passing
  in the language itself; calling an executable built in the language with
  arguments `a b c` should be no different from calling a function from
  within the language with the arguments `a b c` (probably as string values.)

Reflection
----------

* First-class tracebacks.  When a program, for example, encounters an error
  parsing an external file such as a configuration file, it should be able to
  report the position in that file that caused the error as part of the
  traceback, for consistency.  Java has some limited facilities for this, and
  some Python libraries do this (Jinja2? werkzeug?) using frame hacks, but
  a less clumsy solution would be nice.

  Tracebacks are *not* a special case of logging, or an artefact of throwing
  exceptions.  Since the traceback is basically a formatted version of the
  current continuation, this suggests the two facilities should be unified,
  perhaps not totally, but to a high degree.

Abstractions, not Wrappers
--------------------------

The basic principle here is that the existing APIs of most libraries are
(let's be polite) less than ideal, especially when they were designed for
some other language (such as C), and instead of blindly wrapping them in a
new language, the designer should at least *try* to make something nicer.

This applies to very basic facilities as well as what are usually thought
of as external libraries.  In particular, date and time manipulation should
not, by default, simply copycat interfaces like `strftime`, and string
formatting should not simply copycat `printf`.  (The programmer who really
wants interfaces like these can always implement them as "compatibility
modules" if they wish.)

The abstractions should recognize that modern computer systems are generally
not resource-starved (or at least that truly high-level programming languages
should not treat them that way.)  Specifically for date and time, all time
data should be stored consistently, in GMT, always with a time zone.

Serialization
-------------

(This section needs to be rewritten)

- All primitive values must be serializable
- All primitive values must be round-trippable
- All primitive values must thus have an order to them (like Ruby 1.9's
  hashes) because in this world of representations, orderless things don't
  really exist
- When building user-defined values from primitive values it must be
  easy to retain these serialization properties in the composite value
- This is actually fairly agnostic of the particular serialization format
  (yaml, xml, binary, etc)
- S-expressions are trivially serializable, except for functions

Multiple Environments
---------------------

(This section needs to be rewritten)

- Lots of software runs in multiple environments - "development", "qa",
  "production"
- Inherently support that idea
