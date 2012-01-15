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

    > lambda closedEnv (Pair formals (Pair body Null)) = do
    >     return $ Builtin "<lambda>" fun
    >   where
    >     fun env actuals = do
    >         argEnv <- evalArgs formals actuals env
    >         eval (Env.union argEnv closedEnv) body
    >     evalArgs Null Null _ = do
    >         return Env.empty
    >     evalArgs (Pair (Symbol sym) formals) (Pair actual actuals) env = do
    >         x <- eval env actual
    >         rest <- evalArgs formals actuals env
    >         return $ Env.insert sym x rest

Module Definition
-----------------

> bindings = [
>              ("literal",  literal),
>              ("bind",     literal),
>              ("env",      robinEnv),
>              ("let",      literal),
>              ("choose",   literal),
>              ("fun",      literal)
>            ]

> moduleSmall :: IO Expr

> moduleSmall = do
>     core <- moduleCore
>     let small = Env.fromList $ map (\(name,bif) -> (name, Builtin name bif)) bindings
>     return $ Env.union core small

