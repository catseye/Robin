> module Main where

> import System

> import Robin.Parser (parseRobin)
> import Robin.Module (evalRobin, mkModuleCache)

Command-line Entry Point
------------------------

> main = do
>     args <- getArgs
>     processArgs args [] True

> processArgs args nonBuiltinModules printResult =
>     case args of
>         ("-B":moduleName:rest) ->
>             processArgs rest (moduleName:nonBuiltinModules) printResult
>         ("-n":rest) ->
>             processArgs rest nonBuiltinModules False
>         [filename] -> do
>              program <- readFile filename
>              case parseRobin program of
>                  Right ast -> do
>                      (_, result) <- evalRobin (mkModuleCache nonBuiltinModules) ast
>                      case printResult of
>                          True -> do
>                              putStrLn $ show result
>                              exitWith ExitSuccess
>                          False -> do
>                              exitWith ExitSuccess
>                  Left problem -> do
>                      print problem
>                      exitWith $ ExitFailure 1
>         _  -> do
>              putStrLn "Usage: robin [-B module] source.robin"
>              exitWith $ ExitFailure 1
