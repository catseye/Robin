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
> import Robin.CrudeIO

Module Loading
--------------

> modules = [
>             (("core",0,1), moduleCore),
>             (("small",0,1), moduleSmall),
>             (("concurrency",0,1), moduleConcurrency),
>             (("exception",0,1), moduleException),
>             (("crude-io",0,1), moduleCrudeIO)
>           ]

> loadModule :: [String] -> String -> Integer -> Integer -> IO Expr

> loadModule nonBuiltinModules name major minor =
>     case lookup (name, major, minor) modules of
>         Just builtinModule ->
>             if
>                 name `elem` nonBuiltinModules
>               then
>                 loadModuleFromFilesystem nonBuiltinModules name major minor
>               else
>                 builtinModule
>         Nothing -> loadModuleFromFilesystem nonBuiltinModules name major minor

> loadModuleFromFilesystem nonBuiltinModules name major minor =
>     let
>         filename = "module/" ++ name ++ "_" ++ (show major) ++ "_" ++ (show minor) ++ ".robin"
>     in do
>         mod <- readFile filename
>         ast <- return $ insistParse mod
>         evalRobin nonBuiltinModules ast

> loadModules nonBuiltinModules Null = do
>     return Env.empty
> loadModules nonBuiltinModules (Pair (Symbol name) (Pair version rest)) = do
>     (major, minor) <- parseVersion version
>     nextEnv <- loadModules nonBuiltinModules rest
>     thisEnv <- loadModule nonBuiltinModules name major minor
>     return $ Env.union nextEnv thisEnv

> parseVersion (Pair (Number major) (Number minor)) = do
>     case (denominator major, denominator minor) of
>         (1, 1) -> return (numerator major, numerator minor)
>         _      -> error "version number components can't be fractions"

> evalRobin nonBuiltinModules (Pair (Symbol "robin") (Pair version (Pair modules (Pair expr Null)))) = do
>     (major, minor) <- parseVersion version
>     case (major, minor) of
>         (0, 1) -> do
>             initialEnv <- loadModules nonBuiltinModules modules
>             threadId <- myThreadId
>             chan <- newChan
>             let ienv = newIEnv (stop) threadId chan
>             eval initialEnv ienv expr (\x -> do return x)
>         _ -> error ("unsupported language version " ++ show version)

Command-line Entry Point
------------------------

> main = do
>     args <- getArgs
>     processArgs args []

> processArgs args nonBuiltinModules =
>     case args of
>         ("-B":moduleName:rest) ->
>             processArgs rest (moduleName:nonBuiltinModules)
>         [filename] -> do
>              program <- readFile filename
>              case parseRobin program of
>                  Right ast -> do
>                      result <- evalRobin nonBuiltinModules ast
>                      putStrLn $ show result
>                      exitWith ExitSuccess
>                  Left problem -> do
>                      print problem
>                      exitWith $ ExitFailure 1
>         _  -> do
>              putStrLn "Usage: robin [-B module] source.robin"
>              exitWith $ ExitFailure 1
