> module Robin.Metadata where

> import qualified Robin.Env as Env
> import Robin.Expr
> import Robin.Eval

Metadata
========

> robinWith env ienv (List [metaNameExpr, metaValueExpr, expr]) cc =
>     eval env ienv metaNameExpr (\metaName ->
>         eval env ienv metaValueExpr (\metaValue ->
>             eval env ienv expr (\value ->
>                 cc $ Metadata (metaName, metaValue) value)))
> robinWith env ienv other cc = raise ienv (errMsg "illegal-arguments" other)

> robinGet env ienv (List [metaNameExpr, expr]) cc =
>     eval env ienv metaNameExpr (\metaName ->
>         eval env ienv expr (\value ->
>             case getMetadata metaName value of
>                 Just v  -> cc v
>                 Nothing -> cc $ List []))
> robinGet env ienv other cc = raise ienv (errMsg "illegal-arguments" other)

> hasP env ienv (List [metaNameExpr, expr]) cc =
>     eval env ienv metaNameExpr (\metaName ->
>         eval env ienv expr (\value ->
>             cc $ Boolean $ hasMetadata metaName value))
> hasP env ienv other cc = raise ienv (errMsg "illegal-arguments" other)

Module Definition
-----------------

> moduleId = ("metadata", 0, 1)

> moduleDef :: IO Expr
> moduleDef = do
>     return $ Env.fromList $ map (\(name,bif) -> (name, Builtin name bif))
>       [
>         ("with", robinWith),
>         ("has?", hasP),
>         ("get",  robinGet)
>       ]
