> module Main where

> import Data.Ratio

> import System

> import Control.Concurrent (myThreadId)

> import Robin.Expr
> import Robin.Parser
> import Robin.Chan
> import Robin.IEnv
> import qualified Robin.Env as Env
> import Robin.Eval
> import Robin.Core
> import Robin.Small
> import Robin.Concurrency
> import Robin.Exception

Module Loading
--------------

> loadModule :: String -> Integer -> Integer -> IO Expr

> loadModule "core" 0 1 = moduleCore
> -- loadModule "small" 0 1 = moduleSmall
> loadModule "concurrency" 0 1 = moduleConcurrency
> loadModule "exception" 0 1 = moduleException
> loadModule other major minor =
>     let
>         filename = "module/" ++ other ++ "_" ++ (show major) ++ "_" ++ (show minor) ++ ".robin"
>     in do
>         mod <- readFile filename
>         ast <- return $ insistParse mod
>         evalRobin ast

> loadModules Null = do
>     return Env.empty
> loadModules (Pair (Symbol name) (Pair version rest)) = do
>     (major, minor) <- parseVersion version
>     nextEnv <- loadModules rest
>     thisEnv <- loadModule name major minor
>     return $ Env.union nextEnv thisEnv

> parseVersion (Pair (Number major) (Number minor)) = do
>     case (denominator major, denominator minor) of
>         (1, 1) -> return (numerator major, numerator minor)
>         _      -> error "version number components can't be fractions"

> evalRobin (Pair (Symbol "robin") (Pair version (Pair modules (Pair expr Null)))) = do
>     (major, minor) <- parseVersion version
>     case (major, minor) of
>         (0, 1) -> do
>             initialEnv <- loadModules modules
>             threadId <- myThreadId
>             chan <- newChan
>             let ienv = newIEnv (stop) threadId chan
>             eval initialEnv ienv expr (\x -> do return x)
>         _ -> error ("unsupported language version " ++ show version)

Command-line Entry Point
------------------------

> main = do
>     args <- getArgs
>     case args of
>         [filename] -> do
>              program <- readFile filename
>              case parseRobin program of
>                  Right ast -> do
>                      result <- evalRobin ast
>                      putStrLn $ show result
>                      exitWith ExitSuccess
>                  Left problem -> do
>                      print problem
>                      exitWith $ ExitFailure 1
>         _  -> do
>              putStrLn "Usage: robin source.robin"
>              exitWith $ ExitFailure 1
