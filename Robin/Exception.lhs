> module Robin.Exception where

> import Robin.IEnv
> import qualified Robin.Env as Env
> import Robin.Expr
> import Robin.Eval
> import Robin.Core

Exception
=========

> robinCatch env ienv (List [id@(Symbol _), handler, body]) cc =
>     let
>         handlerContinuation = (\errvalue ->
>             eval (Env.insert id errvalue env) ienv handler cc)
>         ienv' = setExceptionHandler handlerContinuation ienv
>     in
>         eval env ienv' body cc
> robinCatch env ienv other cc = raise ienv (errMsg "illegal-arguments" other)

Module Definition
-----------------

> moduleId = ("exception", 0, 1)

> moduleDef :: IO Expr
> moduleDef =
>     return $ Env.fromList $ map (\(name,bif) -> (name, Builtin name bif))
>       [
>         ("catch", robinCatch),
>         ("raise", robinRaise)
>       ]
