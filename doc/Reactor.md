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

Standard Commands
-----------------

### `stop` ###

Stops everything.  This command is provisional.

Standard Facilities
-------------------

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

    | (reactor (line-terminal) 0
    |   (macro (self args env)
    |     (bind event (head args)
    |       (bind event-type (head event)
    |         (if (equal? event-type (literal init))
    |           (list 0
    |             (list (literal writeln) (literal ''Hello, world!''))
    |             (list (literal stop) 0))
    |           (list 0))))))
    = Hello, world!

Reactors which interact with `line-terminal` receive `readln` events.

`readln`, is sent when a line of text is received
on the "standard input".  The payload for this event is a Robin string
of the line of text received.  This string does not contain any end-of-line
marker characters.

Thus we can construct a simple `cat` program:

    | (reactor (line-terminal) 0
    |   (macro (self args env)
    |     (bind event (head args)
    |       (bind event-type (head event)
    |         (bind event-payload (head (tail event))
    |           (if (equal? event-type (literal readln))
    |             (list 0
    |               (list (literal writeln) event-payload))
    |             (list 0)))))))
    + Cat
    + Dog
    = Cat
    = Dog

General Reactor properties
--------------------------

Facilities can handle multiple responses in response to an event.

    | (reactor (line-terminal) 0
    |   (macro (self args env)
    |     (bind event (head args)
    |       (bind event-type (head event)
    |         (bind event-payload (head (tail event))
    |           (if (equal? event-type (literal readln))
    |             (list 0
    |               (list (literal writeln) ''Line:'')
    |               (list (literal writeln) event-payload))
    |             (list 0)))))))
    + Cat
    + Dog
    = Line:
    = Cat
    = Line:
    = Dog

When receiving a malformed response, a facility may produce a warning
message of some kind, but it should otherwise ignore it and keep going.

    | (reactor (line-terminal) 0
    |   (macro (self args env)
    |     (bind event (head args)
    |       (bind event-type (head event)
    |         (bind event-payload (head (tail event))
    |           (if (equal? event-type (literal readln))
    |             (list 0
    |               (literal what-is-this)
    |               (literal i-dont-even)
    |               (list (literal writeln) event-payload))
    |             (list 0)))))))
    + Cat
    + Dog
    = Cat
    = Dog


Reactors can keep state.

    | (define inc (macro (self args env)
    |               (subtract (eval env (head args)) (subtract 0 1))))
    | (reactor (line-terminal) 65
    |   (macro (self args env)
    |     (bind state (head (tail args))
    |       (bind event (head args)
    |         (bind event-type (head event)
    |           (bind event-payload (head (tail event))
    |             (if (equal? event-type (literal readln))
    |               (list (inc state) (list (literal writeln) (list state)))
    |               (list state))))))))
    + Cat
    + Dog
    + Giraffe
    = A
    = B
    = C

This leaves some open questions about reactors (and so their semantics
will definitely change slightly in a subsequent 0.x version of Robin.)
Namely:

*   Can a reactor respond with a `close` to a facility other than the
    facility that sent it the event it is currently handling?
*   Currently reactors cannot communicate with each other at all.
    How can reactors communicate with each other?  (Our idea is to have
    a "reactor bus" facility which can relay responses from one reactor
    into an event for another reactor.)
