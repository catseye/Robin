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

Evaluate a Robin expression in a Robin process.  After the Haskell
process has started, we set up an appropriate IEnv and evaluate
the macro in that.

TODO: should the final continuation send a message to the parent
too?

> spawnExpr :: Expr -> IEnv Expr -> Expr -> IO Expr
> spawnExpr env ienv expr = do
>     spawn launch
>   where
>     launch chan = do
>         thread <- myThreadId
>         let parent = getPid ienv
>         let exch = makeMsgSendingExcHandler parent
>         let myIenv = newIEnv exch thread chan
>         eval env myIenv expr (\x -> do return (List []))
>         return ()

> makeMsgSendingExcHandler pid =
>     \value -> do
>         writeChan (getChan pid) (errMsg "uncaught-exception" value)
>         return (List [])

Capture the "response" pattern for processes which handle `call`s.  This
doesn't require that the process has a Robin pid.

> respond :: Chan Expr -> [(String, a -> Expr -> Expr -> IO (a, Expr))] -> a -> IO ()

> respond chan handlers state = do
>     message <- readChan chan
>     tid <- myThreadId
>     let myPid = Pid tid chan
>     case message of
>         (List [sender, (Symbol tagText), payload]) ->
>             case lookup tagText handlers of
>                 Just handler -> do
>                     (state', reply) <- handler state sender payload
>                     let response = List [myPid, (List [(Symbol tagText), (Symbol "reply")]), reply]
>                     writeChan (getChan sender) response
>                     respond chan handlers state'
>                 Nothing -> do
>                     let response = List [myPid, (List [(Symbol tagText), (Symbol "reply")]), (Symbol "what?")]
>                     writeChan (getChan sender) response
>                     respond chan handlers state
>         _ -> do
>             respond chan handlers state

Robin Functions
---------------

These are the functions exported by this Robin module.

> robinMyself env ienv (List []) cc = do
>     cc $ getPid ienv
> robinMyself env ienv other cc = raise ienv (errMsg "illegal-arguments" other)

> pidP = predP isPid

> robinSpawn env ienv (List [id@(Symbol _), expr, body]) cc = do
>     pid <- spawnExpr env ienv expr
>     eval (Env.insert id pid env) ienv body cc
> robinSpawn env ienv other cc = raise ienv (errMsg "illegal-arguments" other)

> send env ienv (List [pidExpr, msgExpr, body]) cc = do
>     eval env ienv pidExpr (\pid ->
>         assertPid ienv pid (\pid ->
>             eval env ienv msgExpr (\msg -> do
>                 writeChan (getChan pid) msg
>                 eval env ienv body cc)))
> send env ienv other cc = raise ienv (errMsg "illegal-arguments" other)

> recv env ienv (List [id@(Symbol _), body]) cc = do
>    message <- readChan $ getChan $ getPid ienv
>    --putStrLn ((show $ getPid ienv) ++ " just recvd " ++ (show message))
>    eval (Env.insert id message env) ienv body cc
> recv env ienv other cc = raise ienv (errMsg "illegal-arguments" other)

> msgsP env ienv (List []) cc = do
>    isEmpty <- isEmptyChan $ getChan $ getPid ienv
>    cc $ Boolean $ not isEmpty
> msgsP env ienv other cc = raise ienv (errMsg "illegal-arguments" other)

This might, one day, be implemented in Robin, in some other module.
For now, for simplicity, it's here.

`call` is a synchronous communication with another process; it executes a
`send` and then a `recv`.  It only finishes when the message received came
from the process to which the first message was sent; it queues up all other
messages in the meantime, and re-sends them to self when done.

TODO: This should also handle any "finished" or "uncaught exception" response
from the destination pid.

> call env ienv (List [pidExpr, tag@(Symbol _), payloadExpr, repsym@(Symbol _), body]) cc = do
>     eval env ienv pidExpr (\pid ->
>         assertPid ienv pid (\pid ->
>             eval env ienv payloadExpr (\payload -> do
>             let msg = List [(getPid ienv), tag, payload]
>             writeChan (getChan pid) msg
>             waitForResponse env ienv pid tag repsym body [] cc)))
> call env ienv other cc = raise ienv (errMsg "illegal-arguments" other)

> waitForResponse env ienv pid tag repsym body queue cc = do
>     message <- readChan $ getChan $ getPid ienv
>     case message of
>          (List [somePid, (List [someTag, (Symbol "reply")]), returnPayload]) -> do
>              if (pid == somePid) && (tag == someTag) then do
>                  sendAll (getChan $ getPid ienv) (reverse queue)
>                  eval (Env.insert repsym returnPayload env) ienv body cc
>                else
>                  waitForResponse env ienv pid tag repsym body (message:queue) cc
>          (List [(Symbol "uncaught-exception"), excval]) ->
>              raise ienv excval
>          other ->
>              waitForResponse env ienv pid tag repsym body (message:queue) cc

> sendAll chan [] = do
>     return ()
> sendAll chan (msg:msgs) = do
>     writeChan chan msg
>     sendAll chan msgs

> robinRespond env ienv branches cc = do
>     validateRespond ienv branches (\ok -> do
>       message <- readChan $ getChan $ getPid ienv
>       case message of
>         (List [sender, tag@(Symbol _), payload]) -> do
>             case lookupRespondTag tag branches of
>                 Just x@(bindVar, responseExpr, continue) -> do
>                     let newEnv = Env.insert bindVar payload env
>                     eval newEnv ienv responseExpr (\reply -> do
>                         let response = List [(getPid ienv), (List [tag, (Symbol "reply")]), reply]
>                         writeChan (getChan sender) response
>                         eval newEnv ienv continue cc)
>                 -- TODO: this should not necessarily just loop
>                 Nothing -> do
>                     let response = List [(getPid ienv), (List [tag, (Symbol "reply")]), (Symbol "what?")]
>                     writeChan (getChan sender) response
>                     robinRespond env ienv branches cc
>         -- TODO: this should not necessarily just loop
>         _ -> do
>             robinRespond env ienv branches cc)

> validateRespond ienv (List []) cc = cc (List [])
> validateRespond ienv (List ((List [(Symbol _), (List [(Symbol _)]), responseExpr, continue]):rest)) cc =
>     validateRespond ienv (List rest) cc
> validateRespond ienv other cc = raise ienv (errMsg "illegal-arguments" other)

> lookupRespondTag :: Expr -> Expr -> Maybe (Expr, Expr, Expr)

> lookupRespondTag tag (List []) = Nothing
> lookupRespondTag tag (List ((List [candidateTag, (List [bindVar]), responseExpr, continue]):rest))
>     | tag == candidateTag = Just (bindVar, responseExpr, continue)
>     | otherwise           = lookupRespondTag tag (List rest)

Module Definition
-----------------

> moduleId = ("concurrency", 0, 1)

> moduleDef :: IO Expr
> moduleDef = do
>     return $ Env.fromList $ map (\(name,bif) -> (name, Builtin name bif))
>       [
>         ("myself",   robinMyself),
>         ("pid?",     pidP),
>         ("spawn!",   robinSpawn),
>         ("send!",    send),
>         ("recv!",    recv),
>         ("call!",    call),
>         ("respond!", robinRespond),
>         ("msgs?",    msgsP)
>       ]
