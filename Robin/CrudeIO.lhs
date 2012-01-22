> module Robin.CrudeIO where

> import Control.Concurrent (myThreadId)

> import Robin.Chan
> import Robin.Expr
> import qualified Robin.Env as Env
> import Robin.Parser

> import Robin.Concurrency (spawn, getChan)

CrudeIO
=======

A rudimentary I/O module for Robin.

The virtual output device accepts messages, and prints the S-expression
representation of the message to standard output.

> outputHandler :: Chan Expr -> IO ()

> outputHandler chan = do
>     message <- readChan chan
>     case message of
>         (Pair sender (Pair (Symbol "write") (Pair output Null))) -> do
>             putStrLn $ show output
>             tid <- myThreadId
>             let myPid = Pid tid chan
>             let response = (Pair myPid (Pair (Pair (Symbol "write") (Symbol "reply")) (Pair (Symbol "ok") Null)))
>             writeChan (getChan sender) response
>             outputHandler chan
>         (Pair sender (Pair tag rest)) -> do
>             tid <- myThreadId
>             let myPid = Pid tid chan
>             let response = (Pair myPid (Pair (Pair tag (Symbol "reply")) (Pair (Symbol "what?") Null)))
>             writeChan (getChan sender) response
>             outputHandler chan

The virtual input device waits for a line of input to become available,
checks to see if it has any new subscribers (and if so, registers them),
parses the line of text, sends the result to all subscribers (it it could
be parsed), and loops.

If any I/O error is encountered, the virtual input device sends the
symbol `eof` to all of its subscribers, and terminates.

> inputHandler :: Chan Expr -> IO ()

> inputHandler chan = do
>     inputHandler' chan []

> inputHandler' chan subscribers = do
>     line <- getLine `catch` (\e -> return "eof")
>     subscribers' <- getNewSubscribers chan subscribers
>     case parseRobin line of
>         Right expr@(Symbol "eof") -> do
>             sendToSubscribers chan expr subscribers'
>             return ()
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
>     -- putStrLn ("just sent " ++ show expr)
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
