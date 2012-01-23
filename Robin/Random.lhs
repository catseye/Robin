> module Robin.Random where

> import Data.Ratio
> import Control.Concurrent (myThreadId)
> import System.Random (randomRIO)

> import Robin.Chan
> import Robin.Expr
> import qualified Robin.Env as Env

> import Robin.Core (ratFloor)
> import Robin.Concurrency (spawn, getChan, respond)

Random
======

This module could be written in Robin, but solely for my convenience,
it is written in Haskell for now.

> handler :: Chan Expr -> IO ()

> handler chan = respond chan [
>         ("range", \sender (Pair (Number low) (Pair (Number high) Null)) -> do
>             x <- randomRIO ((ratFloor low), (ratFloor high))
>             return $ Number (x % 1))
>     ]

Module Definition
-----------------

> moduleRandom :: IO Expr

> moduleRandom = do
>     randomPid <- spawn handler
>     return $ Env.fromList (
>       [
>         ("random", randomPid)
>       ])
