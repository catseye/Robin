> module Robin.Exception where

> import Robin.IEnv
> import qualified Robin.Env as Env
> import Robin.Expr
> import Robin.Eval
> import Robin.Core

Exception
=========

> robinCatch env ienv (Pair id@(Symbol _) (Pair handler (Pair body Null))) cc =
>     let
>         handlerContinuation = (\errvalue ->
>             eval (Env.insert id errvalue env) ienv handler cc)
>         ienv' = setExceptionHandler handlerContinuation ienv
>     in
>         eval env ienv' body cc
> robinCatch env ienv other cc = raise ienv (
>     Pair (Symbol "illegal-arguments") other)

Module Definition
-----------------

> moduleException :: IO Expr

> moduleException =
>     return $ Env.fromList $ map (\(name,bif) -> (name, Builtin name bif))
>       [
>         ("catch", robinCatch),
>         ("raise", robinRaise)
>       ]
