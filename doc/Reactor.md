Robin Reactors
==============

To separate the concerns of computation and interaction, Robin provides
a construct called a _reactor_.  While evaluation of a Robin expression
accomplishes side-effect-free computation, reactors permit the construction
of so-called "reactive" programs, which are driven by events and may
interact with a user, a remote server on a network, or other source of
events.  Reactors are similar to event handlers in Javascript, or to
processes in Erlang.

In Robin 0.3, a reactor is installed by a top-level form with the syntax
`(reactor TRANSDUCER)`.

The first argument of the `reactor` form is evaluated to obtain a
macro.  This is called the _transducer_ of the reactor.

Whenever an event of interest to the reactor occurs, the transducer is
evaluated, being passed three (pre-evaluated) arguments:

*   A literal symbol called the _event type_, specifying what kind of event
    happened;
*   An arbitrary value called the _event payload_ containing more data about
    the event, in a format specific to that kind of event; and
*   The current state of the reactor.  (This value is undefined
    if the transducer has never before been evaluated.)

Given these things, the transducer is expected to evaluate to a list where the
first element is the new state of the reactor, and each of the subsequent
elements is an _command_, which is itself a two-element list containing:

*   A literal symbol called the _command type_ specifying the kind of
    command that is being requested to be executed; and
*   An arbitrary value called the _command payload_ containing more data
    about the command, in a format specific to that type of command.

There may of course be zero commands in the returned list.  It is an
error if the returned value is not a list containing at least one element.

If the transducer throws an error, no commands will be executed, and
the state of the transducer will remain unchanged.  Implementations
should allow such occurrences to be visible and/or logged.

In fact commands and events may in fact be the same thing.

Standard Events
---------------

### `init` ###

When a reactor first starts up it will receive an event telling it that
it has started up.  The event type for this event is the literal symbol
`init`.

There are two things a reactor will almost always want to do when it
receives an `init` event: establish a known initial state, and
subscribe to facilities.

It establishes a known initial state by returning an initial state
value as the first element of the list it returns.

It subscribes to facilities by returning commands which request
subscription to those facilities.  Once subscribed, it will receive
`subscribed` events from them.  If the facility is not available, it
will receive a `not-available` event.  It may then elect to abort,
or choose an alternate facility, or so forth.

If a reactor isn't subscribed to any facilities, it won't necessarily
receive any events, although this is implementation-specific.

The set of facilities is expected to be largely implementation-specific.

All the same, there will probably be a set of standard facilities.

Let's describe one such facility for concreteness.

Facility `line-terminal`
------------------------

    -> Tests for functionality "Interpret Robin Program (with Small)"

The `line-terminal` facility allows a Robin program to interact with
something or someone over a line-oriented protocol, similar to what
"standard I/O" routed to a terminal gets you under Unix.  Note that
this is not guaranteed to be the "real" standard I/O; it could well be
simulated with modal dialogue boxes in a GUI, or with textareas on a web
page under Javascript, or so forth.

The `line-terminal` facility understands commands of the form

    (writeln <STRING>)

The `<STRING>` argument should be a Robin string (list of integers).  Those
integers, as bytes, are sent to whetever is listening on the other end of
the line terminal.  When attached to an actual terminal console (whether real
or emulated), this would typically cause an ASCII representation of those bytes
to be displayed.

Knowing this, we can write a "Hello, world!" program.  To keep it
simple, we'll simply assume the line-terminal facility exists.
We won't bother to subscribe to it.  In addition, note that this reactor
essentially doesn't keep any state — the initial state of the reactor is simply
the integer 0, and the state is set to 0 after each event is reacted to.

    > | (reactor (macro (self args env)
    > |   (if (equal? (head args) (literal init))
    > |     (list 0)
    > |     (list 0 (list (literal writeln) (literal ''Hello, world!''))))))

Older docs
----------

Reactors which interact with `line-terminal` receive three kinds of events.

The first, `init`, is sent when the facility with which the reactor is
reacting initially becomes ready for use.  In the example above, this is one
of the events actually being reacted to (even though we don't explicitly
check for it.)  To make it explicit, and correct,

    | (reactor (line-terminal) 0 (macro (self args env)
    |   (bind event (head args)
    |     (if (equal? event (literal init))
    |       (list 0 (list (literal writeln) (literal ''Hello, world!'')))
    |       (list 0)))))
    = Hello, world!

The payload for `init` is not yet defined.

Note that the arguments to the reactor body come already-evaluated, so
there's no need to write it as a `fun` or to use `bind-args`.

The second event, `readln`, is sent when a line of text is received
on the "standard input".  The payload for this event is a Robin string
of the line of text received.  This string does not contain any end-of-line
marker characters.

Thus we can construct a simple `cat` program:

    | (reactor (line-terminal) 0 (macro (self args env)
    |   (bind event (head args)
    |     (bind payload (head (tail args))
    |       (if (equal? event (literal readln))
    |         (list 0 (list (literal writeln) payload))
    |         (list 0))))))
    + Cat
    + Dog
    = Cat
    = Dog

The third event, `eof`, is sent when no more input is available
("end of file") on the "standard input".  Perhaps input was redirected
from a file and that file has come to an end, or perhaps the user pressed
Ctrl+D.

The payload for `eof` is not yet defined.

Here is an example of handling all three kinds of events:

    | (reactor (line-terminal) 0 (macro (self args env)
    |   (bind event (head args)
    |     (bind payload (head (tail args))
    |       (choose
    |         ((equal? event (literal init))
    |           (list 0 (list (literal writeln) (literal ''Hello, world!''))))
    |         ((equal? event (literal readln))
    |           (list 0 (list (literal writeln) payload)))
    |         ((equal? event (literal eof))
    |           (list 0 (list (literal writeln) (literal ''Goodbye, world!''))))
    |         (else
    |           (list 0)))))))
    + Cat
    + Dog
    = Hello, world!
    = Cat
    = Dog
    = Goodbye, world!

A reactor accessing the `line-terminal` facility may also make responses in the
form

    (close <ARG> <NEW-STATE>)

This informs the `line-terminal` facility that it is no longer needed by this
reactor, and can stop sending it events.  The `<ARG>` is not yet defined.

So the "Hello, world" example above still isn't quite right; the only reason
it looks right is because the test suite is giving it an empty input, so it
gets an EOF and terminates.  If you run it on the command line, the Robin
implementation will wait for more input after printing "Hello, world!".
(As is its wont.  It's not expected to know that none of its reactors wants
or needs more input.)

To write it properly, in classic hello-world form, we'd have to say

    | (reactor (line-terminal) 0 (macro (self args env)
    |   (bind event (head args)
    |     (if (equal? event (literal init))
    |       (list 0
    |             (list (literal writeln) (literal ''Hello, world!''))
    |             (list (literal close) 0))
    |       (list 0)))))
    = Hello, world!

General Reactor properties
--------------------------

Facilities can handle multiple responses in response to an event.

    | (reactor (line-terminal) 0 (macro (self args env)
    |   (bind event (head args)
    |     (bind payload (head (tail args))
    |       (if (equal? event (literal readln))
    |         (list 0
    |               (list (literal writeln) (literal ''Line:''))
    |               (list (literal writeln) payload))
    |         (list 0))))))
    + Cat
    + Dog
    = Line:
    = Cat
    = Line:
    = Dog

When receiving a malformed response, a facility may produce a warning
message of some kind, but it should otherwise ignore it and keep going.

    | (reactor (line-terminal) 0 (macro (self args env)
    |   (bind event (head args)
    |     (bind payload (head (tail args))
    |       (if (equal? event (literal readln))
    |         (list 0
    |               (literal what-is-this)
    |               (literal i-dont-even)
    |               (list (literal writeln) payload))
    |         (list 0))))))
    + Cat
    + Dog
    = Cat
    = Dog

After a reactor closes, additional responses are ignored.  (Thus, a close
response, if sent, should be last in the list.)

    | (reactor (line-terminal) 0 (macro (self args env)
    |   (bind event (head args)
    |     (bind payload (head (tail args))
    |       (if (equal? event (literal init))
    |         (list 0
    |               (list (literal writeln) (literal ''Hello''))
    |               (list (literal close) 0)
    |               (list (literal writeln) (literal ''there'')))
    |         (list 0))))))
    = Hello

Reactors can keep state.

    | (define inc (macro (self args env)
    |               (subtract (eval env (head args)) (subtract 0 1))))
    | (reactor (line-terminal) 65 (macro (self args env)
    |   (bind event (head args)
    |     (bind payload (head (tail args))
    |       (bind state (head (tail (tail args)))
    |         (if (equal? event (literal readln))
    |           (list (inc state) (list (literal writeln) (list state)))
    |           (list state)))))))
    + Cat
    + Dog
    + Giraffe
    = A
    = B
    = C

Multiple reactors can be installed for a facility.

    | (define inc (macro (self args env)
    |               (subtract (eval env (head args)) (subtract 0 1))))
    | (reactor (line-terminal) 65 (macro (self args env)
    |   (bind event (head args)
    |     (bind payload (head (tail args))
    |       (bind state (head (tail (tail args)))
    |         (if (equal? event (literal readln))
    |           (list (inc state) (list (literal writeln) (list state)))
    |           (list state)))))))
    | (reactor (line-terminal) 0 (macro (self args env)
    |   (bind event (head args)
    |     (bind payload (head (tail args))
    |       (if (equal? event (literal readln))
    |         (list 0 (list (literal writeln) payload))
    |         (list 0))))))
    + Cat
    + Dog
    + Giraffe
    = Cat
    = A
    = Dog
    = B
    = Giraffe
    = C

Reactors react in the *opposite* order they were installed.

    | (define inc (macro (self args env)
    |               (subtract (eval env (head args)) (subtract 0 1))))
    | (reactor (line-terminal) 0 (macro (self args env)
    |   (bind event (head args)
    |     (bind payload (head (tail args))
    |       (if (equal? event (literal readln))
    |         (list 0 (list (literal writeln) payload))
    |         (list 0))))))
    | (reactor (line-terminal) 65 (macro (self args env)
    |   (bind event (head args)
    |     (bind payload (head (tail args))
    |       (bind state (head (tail (tail args)))
    |         (if (equal? event (literal readln))
    |           (list (inc state) (list (literal writeln) (list state)))
    |           (list state)))))))
    + Cat
    + Dog
    + Giraffe
    = A
    = Cat
    = B
    = Dog
    = C
    = Giraffe

Closing one reactor does not stop others.

    | (define inc (macro (self args env)
    |               (subtract (eval env (head args)) (subtract 0 1))))
    | (reactor (line-terminal) 0 (macro (self args env)
    |   (bind event (head args)
    |     (bind payload (head (tail args))
    |       (if (equal? event (literal readln))
    |         (list 0 (list (literal writeln) payload))
    |         (list 0))))))
    | (reactor (line-terminal) 65 (macro (self args env)
    |   (bind event (head args)
    |     (bind payload (head (tail args))
    |       (bind state (head (tail (tail args)))
    |         (if (equal? state 67)
    |           (list state (list (literal close) 0))
    |           (if (equal? event (literal readln))
    |             (list (inc state) (list (literal writeln) (list state)))
    |             (list state))))))))
    + Cat
    + Dog
    + Giraffe
    + Turkey
    + Wallaby
    = A
    = Cat
    = B
    = Dog
    = Giraffe
    = Turkey
    = Wallaby

In fact the `init` event type and the `close` response are not specific
to `line-terminal`, but rather, generic; every facility that a reactor can
react to should send and understand them.

This leaves some open questions about reactors (and so their semantics
will definitely change slightly in a subsequent 0.x version of Robin.)
Namely:
    
*   How does the reactor know which facility the `init` is for?
    (Probably it should be named as part of the payload of `init`?)
*   Can a reactor respond with a `close` to a facility other than the
    facility that sent it the event it is currently handling?
*   Currently reactors cannot communicate with each other at all.
    How can reactors communicate with each other?  (Our idea is to have
    a "reactor bus" facility which can relay responses from one reactor
    into an event for another reactor.)
