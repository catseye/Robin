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
>         ("range", \state sender (List [(Number low), (Number high)]) -> do
>             x <- randomRIO ((ratFloor low), (ratFloor high))
>             return (state, Number (x % 1)))
>     ] ()

Module Definition
-----------------

> moduleId = ("random", 0, 1)

> moduleDef :: IO Expr
> moduleDef = do
>     randomPid <- spawn handler
>     return $ Env.fromList (
>       [
>         ("random", randomPid)
>       ])
