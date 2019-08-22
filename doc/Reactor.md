Robin Reactors
==============

To separate the concerns of computation and interaction, Robin provides
a construct called a _reactor_.  While evaluation of a Robin expression
accomplishes side-effect-free computation, reactors permit the construction
of so-called "reactive" programs, which are driven by events and may
interact with a user, a remote server on a network, or other source of
events.  Reactors are similar to event handlers in Javascript, or to
processes in Erlang.

In Robin 0.3, a reactor is installed by giving a top-level form with the
following syntax:

    (reactor SUBSCRIPTIONS INITIAL-STATE-EXPR TRANSDUCER)

The first argument of the `reactor` form is a literal (unevaluated) list
of symbols, called the _subscriptions_ for the reactor.  Each symbol names
a _facility_ with which the reactor wishes to be able to interact.

The second argument is evaluated, and becomes the _initial state_ of the
reactor.

The third argument of the `reactor` form is evaluated to obtain a
macro.  This is called the _transducer_ of the reactor.

Whenever an event of interest to the reactor occurs, the transducer is
evaluated, being passed two (pre-evaluated) arguments:

*   A two-element list called the _event_.  The elements are:
    *   A literal symbol called the _event type_, specifying what kind of event
        happened.
    *   An arbitrary value called the _event payload_ containing more data about
        the event, in a format specific to the type of event.
*   The current state of the reactor.  (This will be the initial state
    if the reactor body has never before been evaluated.)

Given these things, the transducer is expected to evaluate to a list where the
first element is the new state of the reactor, and each of the subsequent
elements is an _command_, which is itself a two-element list containing:

*   A literal symbol called the _command type_ specifying the kind of
    command that is being requested to be executed; and
*   An arbitrary value called the _command payload_ containing more data
    about the command, in a format specific to that type of command.

There may of course be zero commands in the returned list, but it is an
error if the returned value is not a list containing at least one element.

If the transducer throws an error, no commands will be executed, and
the state of the transducer will remain unchanged.  Implementations
should allow such occurrences to be visible and/or logged.

In fact, commands _are_ events.  We just call them commands when it is
a reactor producing them, and events when a reactor is receiving them.

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

Stops the current reactor, and removes it from the list of active
reactors.  It will no longer receive any events.

Note: not correctly implemented currently.

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
    |               (list (literal writeln) (literal ''Line:''))
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

Subscribing and unsubscribing to facilities
-------------------------------------------

TODO.

Listing a facility in the SUBSCRIPTIONS of the reactor isn't the
only way for a reactor to be notified of events from the facility.
The reactor can also subscribe to the facility at some point after the
reactor has started, and later even unsubscribe from it as well.

Communicating between reactors
------------------------------

It is not really recommended to implement a system with multiple
reactors.  It is better to compose a single large reactor out of
multiple macros.

But currently we allow it, so we should say some words about it.

When a reactor issues a command, all other reactors see it as an
event.
