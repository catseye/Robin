> module Robin.Chan

This module implements channels for message-passing in Robin.  It is
derived from the `[Control.Concurrent.Chan][]` library module from GHC,
and is thus (c)2001 The University of Glasgow, and covered under a
[BSD-style license][].

[Control.Concurrent.Chan]: http://www.haskell.org/ghc/docs/latest/html/libraries/base/src/Control-Concurrent-Chan.html
[BSD-style license]: http://www.haskell.org/ghc/docs/latest/html/libraries/base/LICENSE

It has been modified to associate each channel with exactly one thread, and
it allows only that thread to read values from the channel, unget items to
the channel, or check the channel for emptiness.  This both makes the
message-passing semantics closer to those of Erlang (where each process
has private access to its own message queue), and prevents a [potential
deadlock][] between reading and ungetting (or reading and checking for
emptiness.)

[potential deadlock]: http://hackage.haskell.org/trac/ghc/ticket/4154

>   (
>       Chan,                   -- abstract
>       newChan,                -- :: IO (Chan a)
>       setChanThread,          -- :: Chan a -> ThreadId -> Chan a
>       writeChan,              -- :: Chan a -> a -> IO ()
>       readChan,               -- :: Chan a -> IO a
>       unGetChan,              -- :: Chan a -> a -> IO ()
>       isEmptyChan             -- :: Chan a -> IO Bool
>   ) where

> import Control.Concurrent (ThreadId, myThreadId)
> import Control.Concurrent.MVar

`Chan` is an abstract type representing an unbounded FIFO channel.
A channel is represented by two `MVar`s keeping track of the two ends
of the channel contents,i.e.,  the read- and write ends. Empty `MVar`s
are used to handle consumers trying to read from an empty channel.

> data Chan a
>  = Chan ThreadId
>         (MVar (Stream a))
>         (MVar (Stream a))
>    deriving Eq

> type Stream a = MVar (ChItem a)

> data ChItem a = ChItem a (Stream a)

See the Concurrent Haskell paper for a diagram explaining the
how the different channel operations proceed.

@newChan@ sets up the read and write end of a channel by initialising
these two `MVar`s with an empty `MVar`.

Build and return a new instance of `Chan`.

> newChan :: IO (Chan a)
> newChan = do
>    threadId <- myThreadId
>    hole  <- newEmptyMVar
>    readVar  <- newMVar hole
>    writeVar <- newMVar hole
>    return (Chan threadId readVar writeVar)

Set the thread that is allowed to read the `Chan`.

> setChanThread :: Chan a -> ThreadId -> Chan a
> setChanThread (Chan _ readVar writeVar) threadId =
>   Chan threadId readVar writeVar

To put an element on a channel, a new hole at the write end is created.
What was previously the empty `MVar` at the back of the channel is then
filled in with a new stream element holding the entered value and the
new hole.

Write a value to a `Chan`.

> writeChan :: Chan a -> a -> IO ()
> writeChan (Chan _ _ writeVar) val = do
>   new_hole <- newEmptyMVar
>   modifyMVar_ writeVar $ \old_hole -> do
>     putMVar old_hole (ChItem val new_hole)
>     return new_hole

Read the next value from the 'Chan'.

> readChan :: Chan a -> IO a
> readChan (Chan threadId readVar _) = do
>   me <- myThreadId
>   case threadId == me of
>     True ->
>       modifyMVar readVar $ \read_end -> do
>         (ChItem val new_read_end) <- readMVar read_end
>         return (new_read_end, val)
>     False ->
>       error ((show me) ++ " not allowed to read from this Chan")

Put a data item back onto a channel, where it will be the next item read.

> unGetChan :: Chan a -> a -> IO ()
> unGetChan (Chan threadId readVar _) val = do
>   me <- myThreadId
>   case threadId == me of
>     True -> do
>       new_read_end <- newEmptyMVar
>       modifyMVar_ readVar $ \read_end -> do
>         putMVar new_read_end (ChItem val read_end)
>         return new_read_end
>     False ->
>       error ((show me) ++ " not allowed to unget to this Chan")

Return `True` if the supplied `Chan` is empty.

> isEmptyChan :: Chan a -> IO Bool
> isEmptyChan (Chan threadId readVar writeVar) = do
>   me <- myThreadId
>   case threadId == me of
>     True ->
>       withMVar readVar $ \r -> do
>         w <- readMVar writeVar
>         let eq = r == w
>         eq `seq` return eq
>     False ->
>       error ((show me) ++ " not allowed to check this Chan for emptiness")
