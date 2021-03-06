module Language.Robin.CmdLine where

import System.IO
import System.Exit

import Language.Robin.Expr (Expr(List, Symbol, Abort))
import Language.Robin.Env (mergeEnvs)
import Language.Robin.Parser (parseToplevel, parseExpr)
import Language.Robin.Intrinsics (robinIntrinsics)
import Language.Robin.Builtins (robinBuiltins)
import Language.Robin.TopLevel (initialWorld, destructureWorld, collect, secondaryDefs)


abortWith msg = do
    hPutStrLn stderr msg
    exitWith $ ExitFailure 1


abortWithUsage = do
    abortWith "Usage: robin [--enable-builtins] [--show-events] {[eval] source.robin}"


processFlags flags = processFlags' flags (robinIntrinsics) False where
    processFlags' ("--enable-builtins":rest) env showEvents = processFlags' rest (mergeEnvs robinIntrinsics robinBuiltins) showEvents
    processFlags' ("--show-events":rest) env showEvents = processFlags' rest env True
    processFlags' args env showEvents = (args, env, showEvents)


processArgs args env = processArgs' args $ initialWorld env where
    processArgs' [] world =
        return $ destructureWorld world
    processArgs' ("eval":filename:rest) world = do
        exprText <- readFile filename
        world' <- processRobin (parseExpr exprText) (\expr -> [List [Symbol "display", expr]]) world
        processArgs' rest world'
    processArgs' (filename:rest) world = do
        toplevelText <- readFile filename
        world' <- processRobin (parseToplevel toplevelText) id world
        processArgs' rest world'


processRobin parsed convertToToplevel world =
    case parsed of
        Right expr ->
            return $ collect (convertToToplevel expr) world
        Left problem -> do
            abortWith (show problem)


loadEnv filename env = do
    program <- readFile filename
    world <- processRobin (parseToplevel program) (id) (initialWorld env)
    let (env', _, _) = destructureWorld world
    return (env', secondaryDefs world)


writeResults [] = return ()
writeResults (expr@(Abort _):results) =
    abortWith $ show expr
writeResults (result:results) = do
    putStrLn $ show result
    writeResults results
