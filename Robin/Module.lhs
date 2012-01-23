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

> data ModuleCache = ModuleCache [String] [(ModuleRef, Expr)]

> mkModuleCache nonBuiltinModules = ModuleCache nonBuiltinModules []

> cacheModule :: ModuleCache -> ModuleRef -> Expr -> ModuleCache

> cacheModule mc@(ModuleCache n cachedModules) modRef modExpr =
>     ModuleCache n ((modRef, modExpr):cachedModules)

> builtinModules = [
>             (("core",0,1), moduleCore),
>             (("small",0,1), moduleSmall),
>             (("concurrency",0,1), moduleConcurrency),
>             (("exception",0,1), moduleException),
>             (("crude-io",0,1), moduleCrudeIO),
>             (("random",0,1), moduleRandom)
>           ]

> loadModule :: ModuleCache -> ModuleRef -> IO (ModuleCache, Expr)

> loadModule mc@(ModuleCache nonBuiltinModules cachedModules) modRef@(name, major, minor) =
>     case lookup modRef cachedModules of
>         Just expr -> do
>             return (mc, expr)
>         Nothing ->
>             if name `elem` nonBuiltinModules then
>                 loadModuleFromFilesystem mc modRef
>             else
>                 case lookup modRef builtinModules of
>                     Just builtinModule -> do
>                         expr <- builtinModule
>                         let mc' = cacheModule mc modRef expr
>                         return (mc', expr)
>                     Nothing ->
>                         loadModuleFromFilesystem mc modRef

> loadModuleFromFilesystem :: ModuleCache -> ModuleRef -> IO (ModuleCache, Expr)

> loadModuleFromFilesystem mc@(ModuleCache nonBuiltinModules cachedModules) modRef@(name, major, minor) =
>     let
>         filename = "module/" ++ name ++ "_" ++ (show major) ++ "_" ++ (show minor) ++ ".robin"
>     in do
>         mod <- readFile filename
>         ast <- return $ insistParse mod
>         expr <- evalRobin mc ast
>         return (mc, expr)

> loadModules :: ModuleCache -> Expr -> IO (ModuleCache, Expr)

> loadModules mc Null = do
>     return (mc, Env.empty)
> loadModules mc (Pair (Symbol name) (Pair version rest)) = do
>     (major, minor) <- parseVersion version
>     nextEnv <- loadModules mc rest
>     thisEnv <- loadModule mc (name, major, minor)
>     return (mc, Env.union nextEnv thisEnv)

> parseVersion (Pair (Number major) (Number minor)) = do
>     case (denominator major, denominator minor) of
>         (1, 1) -> return (numerator major, numerator minor)
>         _      -> error "version number components can't be fractions"

> evalRobin mc (Pair (Symbol "robin") (Pair version (Pair modules (Pair expr Null)))) = do
>     (major, minor) <- parseVersion version
>     case (major, minor) of
>         (0, 1) -> do
>             initialEnv <- loadModules mc modules
>             threadId <- myThreadId
>             chan <- newChan
>             let ienv = newIEnv (stop) threadId chan
>             eval initialEnv ienv expr (\x -> do return x)
>         _ -> error ("unsupported language version " ++ show version)
