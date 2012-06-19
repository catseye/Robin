> module Robin.CrudeIO where

> import Control.Concurrent (myThreadId)
> import qualified Control.Exception as Exc

> import Robin.Chan
> import Robin.Expr
> import qualified Robin.Env as Env
> import Robin.Parser

> import Robin.Concurrency (spawn, getChan, respond)

CrudeIO
=======

A rudimentary I/O module for Robin.

The virtual output device accepts messages, and prints the S-expression
representation of the message to standard output.

> outputHandler :: Chan Expr -> IO ()

> outputHandler chan = respond chan [
>         ("write", \state sender payload -> do
>             putStrLn $ show payload
>             return (state, Symbol "ok"))
>     ] ()

The virtual input device waits for a line of input to become available,
checks to see if it has any new subscribers (and if so, registers them),
parses the line of text, sends the result to all subscribers (it it could
be parsed), and loops.

If any I/O error is encountered, the virtual input device sends the
symbol `eof` to all of its subscribers, and goes into a "black hole" loop.
(Note that going into a "black hole" doesn't seem to help race conditions
much.  It's this next thing that has a better effect...)

We wait for at least one subscriber before trying to process any input,
otherwise we might lose input before anyone has subscribed to us.

> inputHandler :: Chan Expr -> IO ()

> inputHandler chan = do
>     inputHandler' chan []

> inputHandler' chan [] = do
>     subscribers <- processNewSubscribers chan []
>     inputHandler' chan subscribers

> inputHandler' chan subscribers = do
>     line <- getLine `Exc.catch` excHandler
>     subscribers' <- getAnyNewSubscribers chan subscribers
>     case parseRobin line of
>         Right expr@(Symbol "eof") -> do
>             sendToSubscribers chan expr subscribers'
>             respond chan [] () -- 'black hole'
>         Right expr -> do
>             sendToSubscribers chan expr subscribers'
>             inputHandler' chan subscribers'
>         Left _ ->
>             inputHandler' chan subscribers'
>   where
>      excHandler :: Exc.SomeException -> IO String
>      excHandler _ = do return "eof"

> getAnyNewSubscribers :: Chan Expr -> [Expr] -> IO [Expr]

> getAnyNewSubscribers chan subscribers = do
>     isEmpty <- isEmptyChan chan
>     case isEmpty of
>         True -> do return subscribers
>         False -> processNewSubscribers chan subscribers

> processNewSubscribers :: Chan Expr -> [Expr] -> IO [Expr]

> processNewSubscribers chan subscribers = do
>     message <- readChan chan
>     case message of
>         (List [sender, (Symbol "subscribe"), _]) -> do
>             tid <- myThreadId
>             let myPid = Pid tid chan
>             let response = List [myPid, (List [(Symbol "subscribe"), (Symbol "reply")]), (Symbol "ok")]
>             writeChan (getChan sender) response
>             getAnyNewSubscribers chan (sender:subscribers)
>         (List (sender:tag:_)) -> do
>             tid <- myThreadId
>             let myPid = Pid tid chan
>             let response = List [myPid, (List [tag, (Symbol "reply")]), (Symbol "what?")]
>             writeChan (getChan sender) response
>             getAnyNewSubscribers chan subscribers
>         _ -> do
>             getAnyNewSubscribers chan subscribers

> sendToSubscribers chan expr [] = do
>     return ()
> sendToSubscribers chan expr (subscriber:rest) = do
>     writeChan (getChan subscriber) expr
>     --putStrLn ("just sent " ++ (show expr) ++ " to " ++ (show subscriber))
>     sendToSubscribers chan expr rest

Module Definition
-----------------

> moduleId = ("crude-io", 0, 1)

> moduleDef :: IO Expr
> moduleDef = do
>     crudeOutputPid <- spawn outputHandler
>     crudeInputPid  <- spawn inputHandler
>     return $ Env.fromList (
>       [
>         ("crude-output", crudeOutputPid),
>         ("crude-input", crudeInputPid)
>       ])
