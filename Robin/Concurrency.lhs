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

First, some general utility functions.  These can be imported by other
modules (especially built-in Robin modules which expose a process.)

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

TODO: the exception handler in the new IEnv should not be `stop`,
but rather something that sends a message to the parent.

TODO: should we put the parent in the IEnv, too?

TODO: should the final continuation send a message to the parent
too?

> spawnMacro :: Expr -> IEnv Expr -> Expr -> IO Expr
> spawnMacro env ienv macro = do
>     spawn launch
>   where
>     launch chan = do
>         thread <- myThreadId
>         let parent = getPid ienv
>         let myIenv = newIEnv (stop) thread chan
>         let expr = Pair macro $ Pair parent Null
>         eval env myIenv expr (\x -> do return Null)
>         return ()

Now the functions exported by this Robin module.

> robinMyself env ienv Null cc = do
>     cc $ getPid ienv
> robinMyself env ienv other cc = raise ienv (Pair (Symbol "illegal-arguments") other)

> pidP = predP isPid

> robinSpawn env ienv (Pair e Null) cc = do
>     eval env ienv e (\macro ->
>         case isMacro macro of
>             True -> do
>                 pid <- spawnMacro env ienv macro
>                 cc $ pid
>             other -> raise ienv (Pair (Symbol "expected-macro") macro))
> robinSpawn env ienv other cc = raise ienv (Pair (Symbol "illegal-arguments") other)

> send env ienv (Pair pidExpr (Pair msgExpr (Pair body Null))) cc = do
>     eval env ienv pidExpr (\pid ->
>         case isPid pid of
>             True ->
>                 eval env ienv msgExpr (\msg -> do
>                     writeChan (getChan pid) msg
>                     eval env ienv body cc)
>             other -> raise ienv (Pair (Symbol "expected-pid") pid))
> send env ienv other cc = raise ienv (Pair (Symbol "illegal-arguments") other)

> recv env ienv (Pair id@(Symbol _) (Pair body Null)) cc = do
>    message <- readChan $ getChan $ getPid ienv
>    eval (Env.insert id message env) ienv body cc
> recv env ienv other cc = raise ienv (Pair (Symbol "illegal-arguments") other)

> msgsP env ienv Null cc = do
>    isEmpty <- isEmptyChan $ getChan $ getPid ienv
>    cc $ Boolean $ not isEmpty
> msgsP env ienv other cc = raise ienv (Pair (Symbol "illegal-arguments") other)

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
>         ("msgs?",    msgsP)
>       ]
