> module Robin.Module where

> import Data.Ratio

> import Control.Concurrent (myThreadId)
> import System.Directory (doesFileExist)

> import Robin.Expr
> import Robin.Parser
> import Robin.Chan
> import Robin.IEnv
> import qualified Robin.Env as Env
> import Robin.Eval

> import Robin.Builtins (builtinModules)

Module Loading
--------------

> type ModuleRef = (String, Integer, Integer)

> data ModuleCache = ModuleCache [String] [String] [ModuleRef] [(ModuleRef, Expr)]

> mkModuleCache modulePath nonBuiltinModules =
>     ModuleCache modulePath nonBuiltinModules [] []

> cacheModule :: ModuleCache -> ModuleRef -> Expr -> ModuleCache

> cacheModule mc@(ModuleCache mp nbi ip cachedModules) modRef modExpr =
>     ModuleCache mp nbi ip ((modRef, modExpr):cachedModules)

> pushModuleInProgress mc@(ModuleCache mp nbi ip c) modRef =
>     ModuleCache mp nbi (modRef:ip) c

> popModuleInProgress mc@(ModuleCache mp nbi (_:ip) c) =
>     ModuleCache mp nbi ip c

> isModuleInProgress mc@(ModuleCache _ _ ip _) modRef =
>     modRef `elem` ip

> qualifyModuleEnv False _ expr =
>     expr
> qualifyModuleEnv True name Null =
>     Null
> qualifyModuleEnv True name (Pair (Pair (Symbol id) val) rest) =
>     Pair (Pair (Symbol (name ++ ":" ++ id)) val) $ qualifyModuleEnv True name rest

> loadModule :: ModuleCache -> ModuleRef -> Bool -> IO (ModuleCache, Expr)

> loadModule mc@(ModuleCache _ nonBuiltinModules _ cachedModules) modRef@(name, major, minor) qualified =
>     case lookup modRef cachedModules of
>         Just expr -> do
>             return (mc, expr)
>         Nothing ->
>             if name `elem` nonBuiltinModules then
>                 loadModuleFromFilesystem mc modRef qualified
>             else
>                 case lookup modRef builtinModules of
>                     Just builtinModule -> do
>                         expr <- builtinModule
>                         let mc' = cacheModule mc modRef expr
>                         let expr' = qualifyModuleEnv qualified name expr
>                         return (mc', expr')
>                     Nothing ->
>                         loadModuleFromFilesystem mc modRef qualified

> loadModuleFromFilesystem :: ModuleCache -> ModuleRef -> Bool -> IO (ModuleCache, Expr)

> loadModuleFromFilesystem mc modRef@(name, major, minor) qualified =
>     if isModuleInProgress mc modRef then
>         error ("circular reference in module " ++ name)
>     else
>         findAndLoadModuleFromFilesystem mc modRef qualified

> findAndLoadModuleFromFilesystem mc@(ModuleCache [] _ _ _) modRef@(name, major, minor) _ =
>     error ("could not locate module file for " ++ (show modRef))

> findAndLoadModuleFromFilesystem mc@(ModuleCache (dir:dirs) nbi ip c) modRef@(name, major, minor) qualified =
>     let
>         filename = dir ++ "/" ++ name ++ "_" ++ (show major) ++ "_" ++ (show minor) ++ ".robin"
>     in do
>         exists <- doesFileExist filename
>         if exists then do
>             mod <- readFile filename
>             ast <- return $ insistParse mod
>             let mc' = pushModuleInProgress mc (name, major, minor)
>             (mc'', expr) <- evalRobin mc' ast
>             let expr' = qualifyModuleEnv qualified name expr
>             let mc''' = popModuleInProgress mc''
>             -- XXX don't we need to call cacheModule here?
>             return (mc''', expr')
>           else
>             findAndLoadModuleFromFilesystem (ModuleCache dirs nbi ip c) modRef qualified

> loadModules :: ModuleCache -> Expr -> IO (ModuleCache, Expr)

> loadModules mc Null = do
>     return (mc, Env.empty)
> loadModules mc (Pair (Pair (Symbol name) (Pair version qualifiers)) rest) = do
>     let qualified = case qualifiers of
>                         (Pair (Symbol "*") Null) -> False
>                         Null                     -> True
>     (major, minor) <- parseVersion version
>     (mc', nextEnv) <- loadModules mc rest
>     (mc'', thisEnv) <- loadModule mc' (name, major, minor) qualified
>     return (mc'', Env.union nextEnv thisEnv)

> parseVersion (Pair (Number major) (Pair (Number minor) Null)) = do
>     case (denominator major, denominator minor) of
>         (1, 1) -> return (numerator major, numerator minor)
>         _      -> error "version number components can't be fractions"

> evalRobin :: ModuleCache -> Expr -> IO (ModuleCache, Expr)

> evalRobin mc (Pair (Symbol "robin") (Pair version (Pair modules (Pair expr Null)))) = do
>     (major, minor) <- parseVersion version
>     case (major, minor) of
>         (0, 1) -> do
>             (mc', initialEnv) <- loadModules mc modules
>             threadId <- myThreadId
>             chan <- newChan
>             let ienv = newIEnv (stop) threadId chan
>             result <- eval initialEnv ienv expr (\x -> do return x)
>             return (mc', result)
>         _ -> error ("unsupported language version " ++ show version)
