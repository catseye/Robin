> module Robin.Concurrency where

> import Control.Concurrent (forkIO, myThreadId)
> import Robin.Chan

> import Robin.IEnv
> import qualified Robin.Env as Env
> import Robin.Expr
> import Robin.Eval
> import Robin.Core

Concurrency
===========

Helper Functions
----------------

These functions can be imported and used by other Haskell modules,
especially built-in Robin modules which expose a process.

Get a Pid representing the current process out of its IEnv.

> getPid :: IEnv Expr -> Expr
> getPid ienv =
>     Pid (getThreadId ienv) (getChannel ienv)

Get the channel of a pid.

> getChan (Pid _ c) = c
> getChan other = error ("getChan: not a Pid: " ++ show other)

Check if an Expr is a pid or not.

> isPid (Pid _ _) = True
> isPid _ = False

> assertPid = assert (isPid) "expected-pid"

Start a Haskell function in a Robin process.  This ensures that the
new process has a chan it can use, and that the current process has
an appropriate reference to that chan as well.  However, it will not
inform the child process who its parent process is.

> spawn :: (Chan Expr -> IO ()) -> IO Expr
> spawn fun = do
>     chan <- newChan
>     thread <- forkIO $ launch (fun) chan
>     let chan' = setChanThread chan thread
>     return $ Pid thread chan'
>   where
>     launch fun chan = do
>         thread <- myThreadId
>         let chan' = setChanThread chan thread
>         fun chan'

Evaluate a Robin macro in a Robin process.  After the Haskell
process has started, we set up an appropriate IEnv and evaluate
the macro in that.

TODO: should the final continuation send a message to the parent
too?

> spawnMacro :: Expr -> IEnv Expr -> Expr -> IO Expr
> spawnMacro env ienv macro = do
>     spawn launch
>   where
>     launch chan = do
>         thread <- myThreadId
>         let parent = getPid ienv
>         let exch = makeMsgSendingExcHandler parent
>         let myIenv = newIEnv exch thread chan (getTrace ienv)
>         let expr = Pair macro $ Pair parent Null
>         eval env myIenv expr (\x -> do return Null)
>         return ()

> makeMsgSendingExcHandler pid =
>     \value -> do
>         writeChan (getChan pid) (Pair (Symbol "uncaught-exception") value)
>         return Null

Capture the "response" pattern for processes which handle `call`s.  This
doesn't require that the process has a Robin pid.

> respond :: Chan Expr -> [(String, Expr -> Expr -> IO Expr)] -> IO ()

> respond chan handlers = do
>     message <- readChan chan
>     tid <- myThreadId
>     let myPid = Pid tid chan
>     case message of
>         (Pair sender (Pair (Symbol tagText) (Pair payload Null))) ->
>             case lookup tagText handlers of
>                 Just handler -> do
>                     reply <- handler sender payload
>                     let response = (Pair myPid (Pair (Pair (Symbol tagText) (Symbol "reply")) (Pair reply Null)))
>                     writeChan (getChan sender) response
>                     respond chan handlers
>                 Nothing -> do
>                     let response = (Pair myPid (Pair (Pair (Symbol tagText) (Symbol "reply")) (Pair (Symbol "what?") Null)))
>                     writeChan (getChan sender) response
>                     respond chan handlers
>         _ -> do
>             respond chan handlers

Robin Functions
---------------

These are the functions exported by this Robin module.

> robinMyself env ienv Null cc = do
>     cc $ getPid ienv
> robinMyself env ienv other cc = raise ienv (Pair (Symbol "illegal-arguments") other)

> pidP = predP isPid

> robinSpawn env ienv (Pair e Null) cc = do
>     eval env ienv e (\macro ->
>         assertMacro ienv macro (\macro -> do
>             pid <- spawnMacro env ienv macro
>             cc $ pid))
> robinSpawn env ienv other cc = raise ienv (Pair (Symbol "illegal-arguments") other)

> send env ienv (Pair pidExpr (Pair msgExpr (Pair body Null))) cc = do
>     eval env ienv pidExpr (\pid ->
>         assertPid ienv pid (\pid ->
>             eval env ienv msgExpr (\msg -> do
>                 writeChan (getChan pid) msg
>                 eval env ienv body cc)))
> send env ienv other cc = raise ienv (Pair (Symbol "illegal-arguments") other)

> recv env ienv (Pair id@(Symbol _) (Pair body Null)) cc = do
>    message <- readChan $ getChan $ getPid ienv
>    --putStrLn ((show $ getPid ienv) ++ " just recvd " ++ (show message))
>    eval (Env.insert id message env) ienv body cc
> recv env ienv other cc = raise ienv (Pair (Symbol "illegal-arguments") other)

> msgsP env ienv Null cc = do
>    isEmpty <- isEmptyChan $ getChan $ getPid ienv
>    cc $ Boolean $ not isEmpty
> msgsP env ienv other cc = raise ienv (Pair (Symbol "illegal-arguments") other)

This might, one day, be implemented in Robin, in some other module.
For now, for simplicity, it's here.

`call` is a synchronous communication with another process; it executes a
`send` and then a `recv`.  It only finishes when the message received came
from the process to which the first message was sent; it queues up all other
messages in the meantime, and re-sends them to self when done.

TODO: This should also handle any "finished" or "uncaught exception" response
from the destination pid.

> call env ienv (Pair pidExpr (Pair tag@(Symbol _) (Pair payloadExpr (Pair repsym@(Symbol _) (Pair body Null))))) cc = do
>     eval env ienv pidExpr (\pid ->
>         assertPid ienv pid (\pid ->
>             eval env ienv payloadExpr (\payload -> do
>             let msg = (Pair (getPid ienv) (Pair tag (Pair payload Null)))
>             debug ienv ("calling", pid, "with", msg)
>             writeChan (getChan pid) msg
>             waitForResponse env ienv pid tag repsym body [] cc)))
> call env ienv other cc = raise ienv (Pair (Symbol "illegal-arguments") other)

> waitForResponse env ienv pid tag repsym body queue cc = do
>     debug ienv "waiting for reply"
>     message <- readChan $ getChan $ getPid ienv
>     debug ienv ("recvd back ", message)
>     case message of
>          (Pair somePid (Pair (Pair someTag (Symbol "reply")) (Pair returnPayload Null))) -> do
>              if (pid == somePid) && (tag == someTag) then do
>                  sendAll (getChan $ getPid ienv) (reverse queue)
>                  eval (Env.insert repsym returnPayload env) ienv body cc
>                else
>                  waitForResponse env ienv pid tag repsym body (message:queue) cc
>          (Pair (Symbol "uncaught-exception") excval) ->
>              raise ienv excval
>          other ->
>              waitForResponse env ienv pid tag repsym body (message:queue) cc

> sendAll chan [] = do
>     return ()
> sendAll chan (msg:msgs) = do
>     --putStrLn ("resending " ++ show msg)
>     writeChan chan msg
>     sendAll chan msgs

> robinRespond env ienv branches cc = do
>     validateRespond ienv branches (\ok -> do
>       debug ienv "waiting to respond"
>       message <- readChan $ getChan $ getPid ienv
>       debug ienv ("responding to", message)
>       case message of
>         (Pair sender (Pair tag@(Symbol _) (Pair payload Null))) -> do
>             case lookupRespondTag tag branches of
>                 Just x@(bindVar, responseExpr, continue) -> do
>                     debug ienv (tag, x)
>                     let newEnv = Env.insert bindVar payload env
>                     eval newEnv ienv responseExpr (\reply -> do
>                         let response = (Pair (getPid ienv) (Pair (Pair tag (Symbol "reply")) (Pair reply Null)))
>                         writeChan (getChan sender) response
>                         debug ienv continue
>                         eval newEnv ienv continue cc)
>                 -- TODO: this should not necessarily just loop
>                 Nothing -> do
>                     debug ienv ("what?", tag)
>                     let response = (Pair (getPid ienv) (Pair (Pair tag (Symbol "reply")) (Pair (Symbol "what?") Null)))
>                     writeChan (getChan sender) response
>                     robinRespond env ienv branches cc
>         -- TODO: this should not necessarily just loop
>         _ -> do
>             robinRespond env ienv branches cc)

> validateRespond ienv Null cc = cc Null
> validateRespond ienv (Pair (Pair (Symbol _) (Pair (Pair (Symbol _) Null) (Pair responseExpr (Pair continue Null)))) rest) cc =
>     validateRespond ienv rest cc
> validateRespond ienv other cc = raise ienv (Pair (Symbol "illegal-arguments") other)

> lookupRespondTag :: Expr -> Expr -> Maybe (Expr, Expr, Expr)

> lookupRespondTag tag Null = Nothing
> lookupRespondTag tag (Pair (Pair candidateTag (Pair (Pair bindVar Null) (Pair responseExpr (Pair continue Null)))) rest)
>     | tag == candidateTag = Just (bindVar, responseExpr, continue)
>     | otherwise           = lookupRespondTag tag rest

> debug :: (Show a) => IEnv Expr -> a -> IO ()

> debug ienv a =
>     if getTrace ienv then
>         print (getThreadId ienv, a)
>       else
>         return ()

Module Definition
-----------------

> moduleConcurrency :: IO Expr

> moduleConcurrency = do
>     return $ Env.fromList $ map (\(name,bif) -> (name, Builtin name bif))
>       [
>         ("myself",   robinMyself),
>         ("pid?",     pidP),
>         ("spawn",    robinSpawn),
>         ("send",     send),
>         ("recv",     recv),
>         ("call",     call),
>         ("respond",  robinRespond),
>         ("msgs?",    msgsP)
>       ]
