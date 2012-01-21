> module Robin.Random where

> import Data.Ratio
> import System.Random (randomRIO)

> import Robin.Chan
> import Robin.Expr
> import qualified Robin.Env as Env

> import Robin.Core (trunc)
> import Robin.Concurrency (spawn, getChan)

Random
======

This module could be written in Robin, but solely for my convenience,
it is written in Haskell for now.

> handler :: Chan Expr -> IO ()

> handler chan = do
>     message <- readChan chan
>     let (Pair sender (Pair (Number low) (Pair (Number high) Null))) = message
>     x <- randomRIO ((trunc low), (trunc high))
>     writeChan (getChan sender) $ Number (x % 1)
>     handler chan

Module Definition
-----------------

> moduleRandom :: IO Expr

TODO: only start the thread if it hasn't been started already.
This is where we could use module caching.

> moduleRandom = do
>     randomPid <- spawn handler
>     return $ Env.fromList (
>       [
>         ("random", randomPid)
>       ])
