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

> robinEnv env ienv Null cc =
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

> choose env ienv (Pair (Pair (Symbol "else") (Pair branch Null)) Null) cc =
>     eval env ienv branch cc
> choose env ienv (Pair (Pair test (Pair branch Null)) rest) cc = do
>     eval env ienv test (\x ->
>         case x of
>             Boolean True ->
>                 eval env ienv branch cc
>             Boolean False ->
>                 choose env ienv rest cc)

> bind env ienv (Pair name@(Symbol _) (Pair expr (Pair body Null))) cc =
>     eval env ienv expr (\value ->
>         eval (Env.insert name value env) ienv body cc)

> robinLet env ienv (Pair bindings (Pair body Null)) cc =
>     bindAll bindings env ienv (\newEnv ->
>         eval newEnv ienv body cc)
>   where
>     bindAll Null env ienv cc =
>         cc env
>     bindAll (Pair (Pair name@(Symbol _) (Pair sexpr Null)) rest) env ienv cc =
>         eval env ienv sexpr (\value ->
>             bindAll rest (Env.insert name value env) ienv cc)

Module Definition
-----------------

> bindings = [
>              ("literal",  literal),
>              ("bind",     bind),
>              ("env",      robinEnv),
>              ("let",      robinLet),
>              ("choose",   choose),
>              ("fun",      robinFun)
>            ]

> moduleSmall :: IO Expr

> moduleSmall = do
>     core <- moduleCore
>     let small = Env.fromList $ map (\(name,bif) -> (name, Builtin name bif)) bindings
>     return $ Env.union core small

