module Main where

import System.IO

import System.Environment
import System.Exit

import Language.Robin.Env (mergeEnvs)
import Language.Robin.Parser (parseRobin)
import Language.Robin.Intrinsics (robinIntrinsics)
import Language.Robin.Builtins (robinBuiltins)
import qualified Language.Robin.TopLevel as TopLevel
import Language.Robin.Reactor (eventLoop, initReactors)


main = do
    args <- getArgs
    case args of
        [] -> do
            putStrLn "Usage: robin [--no-builtins] {source.robin}"
            exitWith $ ExitFailure 1
        ("--no-builtins":rest) -> do
            (env, reactors, results) <- processArgs rest robinIntrinsics [] []
            writeResults $ reverse results
            runReactors reactors
            exitWith ExitSuccess
        _ -> do
            (env, reactors, results) <- processArgs args (mergeEnvs robinIntrinsics robinBuiltins) [] []
            writeResults $ reverse results
            runReactors reactors
            exitWith ExitSuccess


writeResults [] = return ()
writeResults (result:results) = do
    putStrLn $ show result
    writeResults results


runReactors [] = return ()
runReactors reactors = do
    let (reactors', events') = initReactors reactors
    eventLoop reactors' events'


processArgs [] env reactors results = return (env, reactors, results)
processArgs (filename:rest) env reactors results = do
    program <- readFile filename
    case parseRobin program of
        Right topExprs -> do
            (env', reactors', results') <- return $ TopLevel.collect topExprs env reactors results
            processArgs rest env' reactors' results'
        Left problem -> do
            hPutStr stderr (show problem)
            exitWith $ ExitFailure 1
