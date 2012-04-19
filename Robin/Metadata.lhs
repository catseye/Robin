> module Robin.Metadata where

> import qualified Robin.Env as Env
> import Robin.Expr
> import Robin.Eval

Metadata
========

> robinWith env ienv (List [metadataExpr, expr]) cc =
>     eval env ienv metadataExpr (\metadata ->
>         eval env ienv expr (\value ->
>             cc $ Metadata metadata value))
> robinWith env ienv other cc = raise ienv (errMsg "illegal-arguments" other)

> hasP env ienv (List [metadataExpr, expr]) cc =
>     eval env ienv metadataExpr (\metadata ->
>         eval env ienv expr (\value ->
>             cc $ Boolean $ hasMetadata metadata value))
> hasP env ienv other cc = raise ienv (errMsg "illegal-arguments" other)

Module Definition
-----------------

> moduleId = ("metadata", 0, 1)

> moduleDef :: IO Expr
> moduleDef = do
>     return $ Env.fromList $ map (\(name,bif) -> (name, Builtin name bif))
>       [
>         ("with",     robinWith),
>         ("has?",     hasP)
>       ]
