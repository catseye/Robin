> module Robin.CrudeIO where

> import Control.Concurrent (forkIO, myThreadId)
> import Robin.Chan

> import Robin.Expr
> import qualified Robin.Env as Env
> import Robin.Parser

> import Robin.Concurrency (spawn, getChan)

CrudeIO
=======

First cut at a rudimentary I/O module.  The virtual I/O device
accepts messages, and prints the S-expression representation of
the message to standard output.

This is going to be really rough until I figure out how I want
to do this (and how well Haskell will cooperate with me on that.)

> outputHandler :: Chan Expr -> IO ()

> outputHandler chan = do
>     message <- readChan chan
>     let (Pair sender output) = message
>     putStrLn $ show output
>     writeChan (getChan sender) (Symbol "ok")
>     outputHandler chan

> inputHandler :: Chan Expr -> IO ()

> inputHandler chan = do
>     inputHandler' chan []

> inputHandler' chan subscribers = do
>     line <- getLine
>     subscribers' <- getNewSubscribers chan subscribers
>     case parseRobin line of
>         Right expr -> do
>             sendToSubscribers chan expr subscribers'
>             inputHandler' chan subscribers'
>         Left _ ->
>             inputHandler' chan subscribers'

> getNewSubscribers :: Chan Expr -> [Expr] -> IO [Expr]

> getNewSubscribers chan subscribers = do
>     isEmpty <- isEmptyChan chan
>     case isEmpty of
>         True -> do return subscribers
>         False -> do
>             newSubscriber <- readChan chan
>             getNewSubscribers chan (newSubscriber:subscribers)

> sendToSubscribers chan expr [] = do
>     return ()
> sendToSubscribers chan expr (subscriber:rest) = do
>     writeChan (getChan subscriber) expr
>     sendToSubscribers chan expr rest

Module Definition
-----------------

> moduleCrudeIO :: IO Expr

TODO: only start the thread if it hasn't been started already.
This is where we could use module caching.

> moduleCrudeIO = do
>     crudeOutputPid <- spawn outputHandler
>     crudeInputPid  <- spawn inputHandler
>     return $ Env.fromList (
>       [
>         ("crude-output", crudeOutputPid),
>         ("crude-input", crudeInputPid)
>       ])
