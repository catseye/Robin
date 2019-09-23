module Language.Robin.CmdLine where

import System.IO
import System.Exit

import Language.Robin.Expr
import Language.Robin.Parser (parseToplevel, parseExpr)
import Language.Robin.Intrinsics (robinIntrinsics)
import qualified Language.Robin.TopLevel as TopLevel


abortWith msg = do
    hPutStrLn stderr msg
    exitWith $ ExitFailure 1


processFlags ("--no-builtins":rest) env showEvents = processFlags rest robinIntrinsics showEvents
processFlags ("--show-events":rest) env showEvents = processFlags rest env True
processFlags args env showEvents = (args, env, showEvents)


processArgs args env = processArgs' args env [] [] where
    processArgs' [] env reactors results =
        return (env, reactors, results)
    processArgs' ("eval":filename:rest) env reactors results = do
        exprText <- readFile filename
        (env', reactors', results') <- processRobin (parseExpr exprText) (\expr -> [List [Symbol "display", expr]]) env reactors results
        processArgs' rest env' reactors' results'
    processArgs' (filename:rest) env reactors results = do
        toplevelText <- readFile filename
        (env', reactors', results') <- processRobin (parseToplevel toplevelText) id env reactors results
        processArgs' rest env' reactors' results'


processRobin parsed convertToToplevel env reactors results =
    case parsed of
        Right expr -> do
            return $ TopLevel.collect (convertToToplevel expr) env reactors results
        Left problem -> do
            hPutStr stderr (show problem)
            exitWith $ ExitFailure 1


writeResults [] = return ()
writeResults ((Right result):results) = do
    putStrLn $ show result
    writeResults results
writeResults ((Left expr):results) =
    error $ "uncaught exception: " ++ show expr
