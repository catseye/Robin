> module Robin.Small where

> import qualified Robin.Env as Env
> import Robin.Expr
> import Robin.Eval

> import qualified Robin.Core

Small
=====

This implementation of the `small` module is non-normative.  For the
normative definition (in Robin), see `small.robin` in the `module` directory
of the distribution.

> literal env ienv (List (expr:_)) cc =
>     cc expr
> literal env ienv other cc = raise ienv (errMsg "illegal-arguments" other)

> evalAll env ienv [] acc cc =
>     cc $ List $ reverse acc
> evalAll env ienv (head:tail) acc cc =
>     eval env ienv head (\value ->
>         evalAll env ienv tail (value:acc) cc)

> robinList env ienv (List exprs) cc =
>     evalAll env ienv exprs [] cc

> robinEnv env ienv (List _) cc =
>   cc env

> robinFun closedEnv ienv (List [(List formals), body]) cc = do
>     cc $ Builtin "<lambda>" fun
>   where
>     fun env ienv (List actuals) cc = do
>         evalArgs formals actuals actuals env ienv (\argEnv ->
>             eval (Env.union argEnv closedEnv) ienv body cc)
>     evalArgs [] [] _ _ _ cc = do
>         cc Env.empty
>     evalArgs (formal@(Symbol _):formals) (actual:actuals) origActuals env ienv cc = do
>         eval env ienv actual (\value ->
>             evalArgs formals actuals origActuals env ienv (\rest ->
>                 cc $ Env.insert formal value rest))
>     evalArgs _ _ origActuals _ ienv cc = do
>         raise ienv (errMsg "illegal-arguments" (List origActuals))
> robinFun env ienv other cc = raise ienv (errMsg "illegal-arguments" other)

> choose env ienv (List [(List [(Symbol "else"), branch])]) cc =
>     eval env ienv branch cc
> choose env ienv (List ((List [test, branch]):rest)) cc = do
>     eval env ienv test (\x ->
>         case x of
>             Boolean True ->
>                 eval env ienv branch cc
>             Boolean False ->
>                 choose env ienv (List rest) cc)
> choose env ienv other cc = raise ienv (errMsg "illegal-arguments" other)

> bind env ienv (List [name@(Symbol _), expr, body]) cc =
>     eval env ienv expr (\value ->
>         eval (Env.insert name value env) ienv body cc)
> bind env ienv other cc = raise ienv (errMsg "illegal-arguments" other)

> robinLet env ienv (List [(List bindings), body]) cc =
>     bindAll bindings env ienv (\newEnv ->
>         eval newEnv ienv body cc)
>   where
>     bindAll [] env ienv cc =
>         cc env
>     bindAll (List [name@(Symbol _), sexpr]:rest) env ienv cc =
>         eval env ienv sexpr (\value ->
>             bindAll rest (Env.insert name value env) ienv cc)
>     bindAll (other:rest) env ienv cc =
>         raise ienv (errMsg "illegal-binding" other)
> robinLet env ienv other cc = raise ienv (errMsg "illegal-arguments" other)

Module Definition
-----------------

> bindings = [
>              ("literal",  literal),
>              ("list",     robinList),
>              ("bind",     bind),
>              ("env",      robinEnv),
>              ("let",      robinLet),
>              ("choose",   choose),
>              ("fun",      robinFun)
>            ]

> moduleId = ("small", 0, 1)

> moduleDef :: IO Expr
> moduleDef = do
>     core <- Robin.Core.moduleDef
>     let small = Env.fromList $ map (\(name,bif) -> (name, Builtin name bif)) bindings
>     return $ Env.union core small

