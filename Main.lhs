> module Main where

> import System.Environment
> import System.Exit

> import Robin.Parser (parseRobin)
> import Robin.Module (evalRobin, mkModuleCache)

Command-line Entry Point
------------------------

> main = do
>     args <- getArgs
>     processArgs args [] [] True

> processArgs args modulePath nonBuiltinModules printResult =
>     case args of
>         ("-B":moduleName:rest) ->
>             processArgs rest modulePath (moduleName:nonBuiltinModules) printResult
>         ("-m":directoryName:rest) ->
>             processArgs rest (modulePath ++ [directoryName]) nonBuiltinModules printResult
>         ("-n":rest) ->
>             processArgs rest modulePath nonBuiltinModules False
>         [filename] -> do
>              program <- readFile filename
>              case parseRobin program of
>                  Right ast -> do
>                      (_, result) <- evalRobin (mkModuleCache modulePath nonBuiltinModules) ast
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
>              putStrLn "Usage: robin {-B module} {-m dir} [-n] source.robin"
>              exitWith $ ExitFailure 1
