> module Robin.Module where

> import Data.Ratio

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
> import Robin.Random

Module Loading
--------------

> type ModuleRef = (String, Integer, Integer)

> data ModuleCache = ModuleCache [String] [(ModuleRef, IO Expr)]

> mkModuleCache nonBuiltinModules = ModuleCache nonBuiltinModules []

> builtinModules = [
>             (("core",0,1), moduleCore),
>             (("small",0,1), moduleSmall),
>             (("concurrency",0,1), moduleConcurrency),
>             (("exception",0,1), moduleException),
>             (("crude-io",0,1), moduleCrudeIO),
>             (("random",0,1), moduleRandom)
>           ]

> loadModule :: ModuleCache -> ModuleRef -> IO (ModuleCache, Expr)

> loadModule (ModuleCache nonBuiltinModules cachedModules) modRef@(name, major, minor) =
>     case lookup modRef cachedModules of
>         Just module -> module
>         Nothing ->
>             if name `elem` nonBuiltinModules then
>                 loadModuleFromFilesystem nonBuiltinModules modRef
>             case lookup modRef builtinModules of
>                 Just builtinModule ->
>             if
>                 name `elem` nonBuiltinModules
>               then
>                 
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
