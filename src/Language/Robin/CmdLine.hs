module Language.Robin.CmdLine where

import Prelude (id, error, return, show, (++), ($), Bool(True), Either(Left, Right))

import System.IO
import System.Exit

import Language.Robin.Expr (Expr(List, Symbol))
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
        Right expr ->
            let
                world = TopLevel.collect (convertToToplevel expr) TopLevel.World{ TopLevel.env=env, TopLevel.reactors=reactors, TopLevel.results=results }
            in
                return (TopLevel.env world, TopLevel.reactors world, TopLevel.results world)
        Left problem -> do
            abortWith (show problem)


loadEnv filename env = do
    program <- readFile filename
    (env', _, _) <- processRobin (parseToplevel program) id env [] []
    return env'


writeResults [] = return ()
writeResults ((Right result):results) = do
    putStrLn $ show result
    writeResults results
writeResults ((Left expr):results) =
    error $ "uncaught exception: " ++ show expr
