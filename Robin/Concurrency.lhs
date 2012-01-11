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

First, some helpers:

> myself ienv =
>     Pid (getThreadId ienv) (getChannel ienv)

TODO: the initial continuation should not be `stop`, but rather,
something that sends a `oops I bombed` message to the parent pid.

> launch :: Expr -> IEnv Expr -> Chan Expr -> Expr -> IO ()

> launch env ienv chan e = do
>     let expr = (Pair e (Pair (myself ienv) Null))
>     tid <- myThreadId
>     let ienv = newIEnv (stop) tid chan
>     eval env ienv expr (\x -> do return Null)
>     return ()

> getChan (Pid _ c) = c

> isPid (Pid _ _) = True
> isPid _ = False

Now the exported functions.

> robinMyself env ienv Null cc = do
>     cc $ myself ienv
> robinMyself env ienv other cc = raise ienv (Pair (Symbol "illegal-arguments") other)

> pidP = predP isPid

> spawn env ienv (Pair e Null) cc = do
>     chan <- newChan
>     eval env ienv e (\macro ->
>         case isMacro macro of
>             True -> do
>                 threadId <- forkIO (launch env ienv chan macro)
>                 cc $ Pid threadId chan
>             other -> raise ienv (Pair (Symbol "expected-macro") macro))
> spawn env ienv other cc = raise ienv (Pair (Symbol "illegal-arguments") other)

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
>    message <- readChan $ getChan $ myself ienv
>    eval (Env.insert id message env) ienv body cc
> recv env ienv other cc = raise ienv (Pair (Symbol "illegal-arguments") other)

> msgsP env ienv Null cc = do
>    isEmpty <- isEmptyChan $ getChan $ myself ienv
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
>         ("spawn",    spawn),
>         ("send",     send),
>         ("recv",     recv),
>         ("msgs?",    msgsP)
>       ]
