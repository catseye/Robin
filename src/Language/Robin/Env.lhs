> module Language.Robin.Env where

> import Language.Robin.Expr

Environments
============

An environment is an alist which associates symbols with
values (arbitrary S-expressions).

> empty = List []

> insert s@(Symbol _) value env =
>     append (List [List [s, value]]) env

> find s@(Symbol _) (List []) = Nothing
> find s@(Symbol _) (List (List [first, value]:rest))
>     | s == first   = Just value
>     | otherwise    = find s (List rest)

> fromList [] =
>     List []
> fromList ((id, val):xs) =
>     append (List [List [(Symbol id), val]]) (fromList xs)
