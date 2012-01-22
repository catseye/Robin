> module Robin.CrudeIO where

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
>     let (Pair sender output) = message
>     putStrLn $ show output
>     writeChan (getChan sender) (Symbol "ok")
>     outputHandler chan

The virtual input device waits for a line of input to become available,
checks to see if it has any new subscribers (and if so, registers them),
parses the line of text, sends the result to all subscribers (it it could
be parsed), and loops.

> inputHandler :: Chan Expr -> IO ()

> inputHandler chan = do
>     inputHandler' chan []

> inputHandler' chan subscribers = do
>     line <- getLine `catch` (\e -> return "")
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
