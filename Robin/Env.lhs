> module Robin.Env where

> import Robin.IEnv
> import Robin.Expr

Environments
============

An environment is an alist which associates symbols with
values (arbitrary S-expressions).

> empty = List []

> insert s@(Symbol _) value env =
>     append (List [s, value]) env

Merge two environments to yield a new environment.  The merge is
left-biased; entries in the left env override those in the right.

> union (List []) env = env
> union (List (binding:rest)) env =
>     append (List [binding]) (union (List rest) env)

> fromList [] =
>     List []
> fromList ((id, val):xs) =
>     append (List [(Symbol id), val]) (fromList xs)
