Robin Reactors
==============

To seperate the concerns of computation and interaction, Robin provides
a construct called a _reactor_.  While normal S-expression evaluation
accomplishes side-effect-free computation, reactors permit the construction
of interactive programs.  Reactors are similar to event handlers in
languages such as Javascript, or to `gen_server`s in Erlang.

In Robin, a reactor is installed by a top-level form with the syntax
`(reactor LIST-OF-SYMBOLS STATE-EXPR BODY-EXPR)`.

The first argument of the `reactor` form is a literal (unevaluated) list
of symbols, called the _subscriptions_ for the reactor.  Each symbol names
a _facility_ with which the reactor wishes to be able to interact.

The second argument is evaluated, and becomes the _initial state_ of the
reactor.

The third argument is evaluated, presumably to a macro.  This is called the
_body_ of the reactor.

Whenever an event of interest to the reactor (as determined by the facilities
with which the reactor requested interaction) occurs, the body is evaluated,
being passed three (pre-evaluated) arguments:
    
*   A literal symbol called the _event code_, specifying what kind of event
    happened;
*   An arbitrary value called the _event payload_ containing more data about
    the event, in a format specific to that kind of event; and
*   The previous state of the reactor.  (This will be the initial state
    if the reactor body has never before been evaluated.)

The body is expected to return a list where the first element is the new
state of the reactor, and each of the subsequent elements is a _response_
to the facility.  A response it iself a two-element list containing:
    
*   A literal symbol called the _response code_ specifying the kind of
    response to the event that is being made; and
*   An arbitrary value called the _response payload_ containing more data
    about the response, in a format specific to that kind of response.

There may of course be zero responses in the returned list.  If the
returned value is not a list containing at least one element, no responses
will be made and the state of the reactor will remain unchanged.

It will be difficult to provide examples of how to use reactors without
introducing a concrete facility to react with, so we'll do that now.

Facility `line-terminal`
------------------------

    -> Tests for functionality "Interpret Robin Program (with Small)"

The `line-terminal` facility allows a Robin program to interact with a
terminal-based, line-buffered "standard I/O" a la Unix.  Note that there is
nothing in the Robin language that requires this to be "the real standard
I/O"; Robin denies any knowledge of that sort of thing.  It could well be
simulated with modal dialogue boxes in a GUI, or with textareas on an web
page under Javascript.

A reactor accessing the `line-terminal` facility may make responses in the
form

    (writeln <STRING> <NEW-STATE>)

The `<STRING>` argument should be a Robin string (list of integers).  Those
integers, as bytes, are sent to something that resembles the "standard output"
under Unix.  (When attached to a terminal, this would typically cause an
ASCII representation of those bytes to be displayed.)

In the following example, the string is printed multiple times because the
reactor is reacting indiscriminately to multiple events — we'll get to that
in a second.  In addition, note that this reactor essentially doesn't keep
any state — the initial state of the reactor is simply the integer 0, and
the state is set to 0 after each event is reacted to.

    | (reactor (line-terminal) 0 (macro (self args env)
    |   (list 0 (list (literal writeln) (literal ''Hello, world!'')))))
    = Hello, world!
    = Hello, world!

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
*   Can a reactor response with a `close` to a facility other than the
    facility that sent it the event it is currently handling?
*   Currently reactors cannot communicate with each other at all.
    How can reactors communicate with each other?  (Our idea is to have
    a "reactor bus" facility which can relay responses from one reactor
    into an event for another reactor.)
