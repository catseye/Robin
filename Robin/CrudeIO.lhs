> module Robin.CrudeIO where

> import Control.Concurrent (forkIO, myThreadId)
> import Robin.Chan

> import Robin.Expr
> import qualified Robin.Env as Env

> import Robin.Concurrency (spawn, getChan)

CrudeIO
=======

First cut at a rudimentary I/O module.  The virtual I/O device
accepts messages, and prints the S-expression representation of
the message to standard output.

This is going to be really rough until I figure out how I want
to do this (and how well Haskell will cooperate with me on that.)

> handler :: Chan Expr -> IO ()

> handler chan = do
>    message <- readChan chan
>    let (Pair sender output) = message
>    putStr $ show output
>    writeChan (getChan sender) (Symbol "ok")
>    handler chan

TODO: Need a seperate thread for handling input here.

Module Definition
-----------------

> moduleCrudeIO :: IO Expr

TODO: only start the thread if it hasn't been started already.
This is where we could use module caching.

> moduleCrudeIO = do
>     crudeIOpid <- spawn handler
>     return $ Env.fromList (
>       [
>         ("crude-io", crudeIOpid)
>       ])
