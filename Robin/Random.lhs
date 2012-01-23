> module Robin.Random where

> import Data.Ratio
> import Control.Concurrent (myThreadId)
> import System.Random (randomRIO)

> import Robin.Chan
> import Robin.Expr
> import qualified Robin.Env as Env

> import Robin.Core (ratFloor)
> import Robin.Concurrency (spawn, getChan)

Random
======

This module could be written in Robin, but solely for my convenience,
it is written in Haskell for now.

> handler :: Chan Expr -> IO ()

> handler chan = do
>     message <- readChan chan
>     case message of
>         (Pair sender (Pair (Symbol "range") (Pair (Pair (Number low) (Pair (Number high) Null)) Null))) -> do
>             x <- randomRIO ((ratFloor low), (ratFloor high))
>             tid <- myThreadId
>             let myPid = Pid tid chan
>             let response = (Pair myPid (Pair (Pair (Symbol "range") (Symbol "reply")) (Pair (Number (x % 1)) Null)))
>             writeChan (getChan sender) response
>             handler chan
>         (Pair sender (Pair tag rest)) -> do
>             tid <- myThreadId
>             let myPid = Pid tid chan
>             let response = (Pair myPid (Pair (Pair tag (Symbol "reply")) (Pair (Symbol "what?") Null)))
>             writeChan (getChan sender) response
>             handler chan
>         _ -> do
>             handler chan

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
