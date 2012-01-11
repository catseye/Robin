> module Robin.Env where

> import Robin.IEnv
> import Robin.Expr

Environments
============

An environment is an alist which associates symbols with
values (arbitrary S-expressions).

> empty = Null

> insert s@(Symbol _) value env =
>     Pair (Pair s value) env

Merge two environments to yield a new environment.  The merge is
left-biased; entries in the left env override those in the right.

> union Null env = env
> union (Pair binding rest) env =
>     Pair binding (union rest env)

> fromList [] =
>     Null
> fromList ((id, val):xs) =
>     Pair (Pair (Symbol id) val) (fromList xs)
