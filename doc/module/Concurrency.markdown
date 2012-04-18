Robin - Concurrency Module
==========================

    -> Tests for functionality "Interpret Robin Program"

The `concurrency` module exports macros for working with concurrently-
executing processes which communicate with each other via message-
passing.

Functionality in this module is difficult to test in isolation, so
many of the tests make use of more than one macro from this module.

Data Types
----------

### Process Identifiers ###

A process identifier is an opaque value which identifies a process.

Also known as "pids".

Pids cannot be denoted directly in the textual S-expression format.
Several macros in `concurrency` evaluate to a pid, however.

Functions
---------

Robin's `concurrency` module exports things for managing concurrently-
executing processes.

### `myself` ###

`myself` takes no arguments and evaluates to the pid of the currently
running process.

`myself` expects exactly zero arguments.

    | (robin (0 1) ((small (0 1) *) (concurrency (0 1) *))
    |   (myself 123))
    ? uncaught exception: (illegal-arguments (123))

### `pid?` ###

`pid?` evaluates its argument, then evaluates to `#t` if that value
is a process identifier, `#f` otherwise.

    | (robin (0 1) ((small (0 1) *) (concurrency (0 1) *))
    |   (pid? (literal b)))
    = #f

    | (robin (0 1) ((small (0 1) *) (concurrency (0 1) *))
    |   (pid? (myself)))
    = #t

The argument to `pid?` may naturally be of any type, but there
must be exactly one argument.

    | (robin (0 1) ((small (0 1) *) (concurrency (0 1) *))
    |   (pid?))
    ? uncaught exception: (illegal-arguments ())

    | (robin (0 1) ((small (0 1) *) (concurrency (0 1) *))
    |   (pid? 200 500))
    ? uncaught exception: (illegal-arguments (200 500))

### `spawn!` ###

`spawn!` starts a concurrent process and evaluates its second argument in
that new process.  It binds the the process identifier (pid) of the new
process to its first argument, which should be an identifier.  It then
proceeds to evaluate its third argument, in the current process, with
that new binding.

  | (robin (0 1) ((small (0 1) *) (concurrency (0 1) *))
  |   (spawn! worker (literal ok)
  |     (pid? worker)))
  = #t

  | (robin (0 1) ((small (0 1) *) (concurrency (0 1) *))
  |   (spawn! worker (literal ok)
  |     (equal? (myself) worker)))
  = #f

`spawn!` takes exactly three arguments.

    | (robin (0 1) ((small (0 1) *) (concurrency (0 1) *))
    |   (spawn!))
    ? uncaught exception: (illegal-arguments ())

    | (robin (0 1) ((small (0 1) *) (concurrency (0 1) *))
    |   (spawn! cheesecake))
    ? uncaught exception: (illegal-arguments (cheesecake))

    | (robin (0 1) ((small (0 1) *) (concurrency (0 1) *))
    |   (spawn! (fun (x) 0) (fun (x) 1)))
    ? uncaught exception: (illegal-arguments ((fun (x) 0) (fun (x) 1)))

    | (robin (0 1) ((small (0 1) *) (concurrency (0 1) *))
    |   (spawn! (fun (x) 0) (fun (x) 1)
    |          (fun (x) 2) (fun (x) 3)))
    ? uncaught exception: (illegal-arguments ((fun (x) 0) (fun (x) 1) (fun (x) 2) (fun (x) 3)))

The first argument to `spawn!` must be a symbol.

    | (robin (0 1) ((small (0 1) *) (concurrency (0 1) *))
    |   (spawn! #f (literal ok) 123))
    ? uncaught exception: (illegal-arguments (#f (literal ok) 123))

### `send!` ###

`send!` evaluates its first argument to obtain a pid, then its second
argument to obtain a value.  It then sends that value as a message to
the process identified by the pid, then evaluates its third argument
and itself evaluates to that.

    | (robin (0 1) ((small (0 1) *) (concurrency (0 1) *))
    |   (spawn! worker (literal ok)
    |     (send! worker (literal spam) (literal ok))))
    = ok

`send!` expects its first argument to be a pid.

    | (robin (0 1) ((small (0 1) *) (concurrency (0 1) *))
    |   (send! (literal eggs) (literal spam) (literal ok)))
    ? uncaught exception: (expected-pid eggs)

`send!` expects exactly three arguments.

    | (robin (0 1) ((small (0 1) *) (concurrency (0 1) *))
    |   (spawn! worker (literal ok)
    |     (send! worker worker)))
    ? uncaught exception: (illegal-arguments (worker worker))

    | (robin (0 1) ((small (0 1) *) (concurrency (0 1) *))
    |   (spawn! worker (literal ok)
    |     (send! worker worker worker worker)))
    ? uncaught exception: (illegal-arguments (worker worker worker worker))

### `recv!` ###

`recv!` waits for a message to arrive in the currently executing
process's queue, and removes it.  It binds the identifier given in
its first argument to the value so received, and evaluates its second
argument, and itself evaluates to that.

    | (robin (0 1) ((small (0 1) *) (concurrency (0 1) *))
    |   (bind parent (myself)
    |     (spawn! worker (send! parent (literal lettuce) (literal ok))
    |       (recv! message (list message message)))))
    = (lettuce lettuce)

`recv!` expects its first argument to be an identifier to be bound.  (This
is a case of illegal arguments, as the identifier is not an expression
that must evaluate to a certain type.)

    | (robin (0 1) ((small (0 1) *) (concurrency (0 1) *))
    |   (bind parent (myself)
    |     (spawn! worker (send! parent (literal lettuce) (literal ok))
    |       (recv! (list 7) 9))))
    ? uncaught exception: (illegal-arguments ((list 7) 9))

`recv!` expects exactly two arguments.

    | (robin (0 1) ((small (0 1) *) (concurrency (0 1) *))
    |   (bind parent (myself)
    |     (spawn! worker (send! parent (literal lettuce) (literal ok))
    |       (recv! message))))
    ? uncaught exception: (illegal-arguments (message))

    | (robin (0 1) ((small (0 1) *) (concurrency (0 1) *))
    |   (bind parent (myself)
    |     (spawn! worker (send! parent (literal lettuce) (literal ok))
    |       (recv! message message message))))
    ? uncaught exception: (illegal-arguments (message message message))

Tests for both `send!` and `recv!` follow.

A process we spawned can send our message back to us.

    | (robin (0 1) ((small (0 1) *) (concurrency (0 1) *))
    |   (bind parent (myself)
    |     (spawn! worker (recv! message (send! parent message 123))
    |       (send! worker (literal whoopie)
    |         (recv! message message)))))
    = whoopie

A process we spawned can receive multiple messages.

    | (robin (0 1) ((small (0 1) *) (concurrency (0 1) *))
    |   (bind parent (myself)
    |     (spawn! worker 
    |       (recv! message1
    |         (recv! message2
    |           (send! parent (list message1 message2) 0)))
    |       (send! worker (literal thats)
    |         (send! worker (literal entertainment)
    |           (recv! message message))))))
    = (thats entertainment)

A process we spawned will terminate while waiting for a message, if the
main process terminates.

    | (robin (0 1) ((small (0 1) *) (concurrency (0 1) *))
    |   (bind parent (myself)
    |     (spawn! worker
    |       (recv! message1
    |         (recv! message2
    |           (send! parent (list message1 message2) 0)))
    |       (send! worker (literal thats)
    |         (literal stop)))))
    = stop

A spawned process can spawn processes of its own.

    | (robin (0 1) ((small (0 1) *) (concurrency (0 1) *))
    |   (bind parent (myself)
    |     (spawn! worker
    |       (bind subparent (myself)
    |         (spawn! subworker (send! subparent (myself) 123)
    |           (recv! message (send! parent message 123))))
    |       (recv! subworker
    |          (list (pid? subworker) (equal? worker subworker))))))
    = (#t #f)

A process can send messages to any process it knows about, not just
its parent.

    | (robin (0 1) ((small (0 1) *) (concurrency (0 1) *))
    |   (bind parent (myself)
    |     (spawn! worker
    |       (spawn! subworker
    |         (send! parent (literal hello) 123) 12345)
    |     (recv! message message))))
    = hello

If an exception is raised, but not caught, in a process, that process
sends a message to this effect to the process that spawned it,
immediately before terminating.

    | (robin (0 1) ((small (0 1) *) (concurrency (0 1) *))
    |   (bind parent (myself)
    |     (spawn! worker
    |       (bind x (head ())
    |         (send! parent (literal hi) 0))
    |       (recv! message message))))
    = (uncaught-exception (expected-list ()))

### `msgs?` ###

`msgs?` evaluates to `#t` if the current process has one or more messages
waiting in its queue, `#f` otherwise.

    | (robin (0 1) ((small (0 1) *) (concurrency (0 1) *))
    |   (msgs?))
    = #f

Note: it's hard to write this test without a race condition...

    X | (robin (0 1) ((small (0 1) *) (concurrency (0 1) *))
    X |   (bind parent (myself)
    X |     (spawn! (send! parent (literal lettuce) (literal ok)) worker
    X |       (msgs?))))
    X = #t

`msgs?` expects exactly zero arguments.

    | (robin (0 1) ((small (0 1) *) (concurrency (0 1) *))
    |   (msgs? #t))
    ? uncaught exception: (illegal-arguments (#t))

### `call!` ###

`call!` combines `send!` and `recv!` to accomplish synchronous communication
with another process.

    | (robin (0 1) ((small (0 1) *) (concurrency (0 1) *))
    |   (spawn! worker
    |     (recv! message
    |       (let ((sender  (head message))
    |             (tag     (head (tail message)))
    |             (payload (head (tail (tail message)))))
    |         (send! sender (list (myself)
    |                             (list tag (literal reply))
    |                             (list tag payload)) 0)))
    |     (call! worker this-tag (literal this-payload) reply
    |       (list (literal reply-was) reply))))
    = (reply-was (this-tag this-payload))

The pid and tag in the return message must match, or `call!` will
not finish.  Note: this is an awful test, because an implementation
may in fact hang instead of doing what our implementation does.

    | (robin (0 1) ((small (0 1) *) (concurrency (0 1) *))
    |   (bind parent (myself) (spawn! worker
    |     (recv! message
    |       (let ((sender  (head message))
    |             (tag     (head (tail message)))
    |             (payload (head (tail (tail message)))))
    |         (send! sender (list parent
    |                             (list tag (literal reply))
    |                             (list tag payload ())) 0)))
    |      (call! worker this-tag (literal this-payload) reply
    |        (list (literal reply-was) reply)))))
    ? thread blocked indefinitely in an MVar operation

    | (robin (0 1) ((small (0 1) *) (concurrency (0 1) *))
    |   (bind parent (myself) (spawn! worker
    |     (recv! message
    |       (let ((sender  (head message))
    |             (tag     (head (tail message)))
    |             (payload (head (tail (tail message)))))
    |         (send! sender (list (myself)
    |                             (list (literal some-other-tag) (literal reply))
    |                             (list tag payload ())) 0)))
    |      (call! worker this-tag (literal this-payload) reply
    |        (list (literal reply-was) reply)))))
    ? thread blocked indefinitely in an MVar operation

If `call!` receives an exception message, it will `raise` that
exception.  Note that I'm not sure if we actually want this
behavior or what (it's an exception message from any process,
currently!) but... we'll see.

    | (robin (0 1) ((small (0 1) *) (concurrency (0 1) *))
    |   (spawn! worker
    |     (recv! message
    |       (let ((sender  (head message))
    |             (tag     (head (tail message)))
    |             (payload (head (tail (tail message)))))
    |         (head (literal argh))))
    |     (call! worker this-tag (literal this-payload) reply
    |       (list (literal reply-was) reply))))
    ? uncaught exception: (expected-list argh)

`call!` expects its first argument to be a pid.

    | (robin (0 1) ((small (0 1) *) (concurrency (0 1) *))
    |   (call! (literal worker) this-tag (literal this-payload) reply
    |      (list (literal reply-was) reply)))
    ? uncaught exception: (expected-pid worker)

`call!` expects its second argument to be a symbol.

    | (robin (0 1) ((small (0 1) *) (concurrency (0 1) *))
    |   (call! (myself) #f (literal this-payload) reply
    |      (list (literal reply-was) reply)))
    ? uncaught exception: (illegal-arguments ((myself) #f (literal this-payload) reply (list (literal reply-was) reply)))

`call!` expects its fourth argument to be an identifier.

    | (robin (0 1) ((small (0 1) *) (concurrency (0 1) *))
    |   (call! (myself) this-tag (literal this-payload) #f
    |      (list (literal reply-was) reply)))
    ? uncaught exception: (illegal-arguments ((myself) this-tag (literal this-payload) #f (list (literal reply-was) reply)))

### `respond!` ###

`respond!` is the counterpart to `call!`.  It assumes the curren process is
waiting to be call!ed by `call!` from another process, and when it receives
such a message, it acts like a case statement against the tag.  It evaluates
the appropriate branch, sends a reply message to the pid that `call!`ed it,
and evaluates the tail of the branch.

    | (robin (0 1) ((small (0 1) *) (concurrency (0 1) *))
    |   (spawn! worker
    |     (respond!
    |       (donkey (x) (literal kong)   (literal ok))
    |       (monkey (x) (literal island) (literal ok)))
    |     (call! worker donkey () reply reply)))
    = kong

The payload of the `call!` is available in the bound variable.

    | (robin (0 1) ((small (0 1) *) (concurrency (0 1) *))
    |   (spawn! worker
    |     (respond!
    |       (donkey (x) (literal kong)            (literal ok))
    |       (monkey (x) (list (literal island) x) (literal ok)))
    |     (call! worker monkey (literal shines) reply reply)))
    = (island shines)

`respond!` responds to only one message.

    | (robin (0 1) ((small (0 1) *) (concurrency (0 1) *))
    |   (spawn! worker
    |     (respond!
    |       (donkey (x) (literal kong)            (literal ok))
    |       (monkey (x) (list (literal island) x) (literal ok)))
    |     (call! worker donkey () reply
    |       (call! worker monkey (literal shines) reply reply))))
    ? thread blocked indefinitely in an MVar operation

Typically, to write a server, you would use it in a loop.

    | (robin (0 1) ((small (0 1) *) (concurrency (0 1) *))
    |   (bind work-fun (fun (self)
    |     (respond!
    |       (donkey (x) (literal kong)            (self self))
    |       (monkey (x) (list (literal island) x) (self self))
    |       (stop   (x) (literal ok)              (literal ok))))
    |     (spawn! worker (work-fun work-fun)
    |       (call! worker donkey () reply1
    |         (call! worker monkey (literal shines) reply2
    |           (call! worker stop 123 reply3
    |             (list reply1 reply2 reply3)))))))
    = (kong (island shines) ok)

`respond!` evaluates to the appropriate tail.

    | (robin (0 1) ((small (0 1) *) (concurrency (0 1) *))
    |   (bind parent (myself) (spawn! worker
    |     (call! parent take 71 reply (literal ok))
    |     (respond!
    |       (take (x) (literal ok) (list 55 x))))))
    = (55 71)
