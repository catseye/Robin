> module Robin.Small where

> import qualified Robin.Env as Env
> import Robin.Expr
> import Robin.Eval

> import Robin.Core

Small
=====

This implementation of the `small` module is non-normative.

> literal env ienv (Pair expr Null) cc =
>   cc expr
> literal env ienv other cc = raise ienv (Pair (Symbol "illegal-arguments") other)

> robinEnv env ienv (Pair expr Null) cc =
>   cc env
> robinEnv env ienv other cc = raise ienv (Pair (Symbol "illegal-arguments") other)

> robinFun closedEnv ienv (Pair formals (Pair body Null)) cc = do
>     cc $ Builtin "<lambda>" fun
>   where
>     fun env ienv actuals cc = do
>         evalArgs formals actuals env ienv (\argEnv ->
>             eval (Env.union argEnv closedEnv) ienv body cc)
>     evalArgs Null Null _ _ cc = do
>         cc Env.empty
>     evalArgs (Pair sym@(Symbol _) formals) (Pair actual actuals) env ienv cc = do
>         eval env ienv actual (\value ->
>             evalArgs formals actuals env ienv (\rest ->
>                 cc $ Env.insert sym value rest))

Old...

    > cond env rest =
    >     checkAll env rest
    >   where
    >     checkAll env (Pair (Pair (Symbol "else") (Pair branch Null)) Null) =
    >         eval env branch
    >     checkAll env (Pair (Pair test (Pair branch Null)) rest) = do
    >         x <- eval env test
    >         case x of
    >             Boolean True ->
    >                 eval env branch
    >             Boolean False ->
    >                 checkAll env rest


Module Definition
-----------------

> bindings = [
>              ("literal",  literal),
>              ("bind",     literal),
>              ("env",      robinEnv),
>              ("let",      literal),
>              ("choose",   literal),
>              ("fun",      robinFun)
>            ]

> moduleSmall :: IO Expr

> moduleSmall = do
>     core <- moduleCore
>     let small = Env.fromList $ map (\(name,bif) -> (name, Builtin name bif)) bindings
>     return $ Env.union core small

