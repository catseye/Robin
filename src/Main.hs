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
            abortWith "Usage: robin [--no-builtins] [--show-events] {source.robin}"
        _ -> do
            let (args', env', showEvents) = processFlags args (mergeEnvs robinIntrinsics robinBuiltins) False
            (_, reactors, results) <- processArgs args' env'
            writeResults $ reverse results
            runReactors reactors showEvents
            exitWith ExitSuccess


abortWith msg = do
    hPutStrLn stderr msg
    exitWith $ ExitFailure 1


processFlags ("--no-builtins":rest) env showEvents = processFlags rest robinIntrinsics showEvents
processFlags ("--show-events":rest) env showEvents = processFlags rest env True
processFlags args env showEvents = (args, env, showEvents)


processArgs args env = processArgs' args env [] [] where
    processArgs' [] env reactors results = return (env, reactors, results)
    processArgs' (filename:rest) env reactors results = do
        program <- readFile filename
        case parseRobin program of
            Right topExprs -> do
                (env', reactors', results') <- return $ TopLevel.collect topExprs env reactors results
                processArgs' rest env' reactors' results'
            Left problem -> do
                hPutStr stderr (show problem)
                exitWith $ ExitFailure 1


writeResults [] = return ()
writeResults (result:results) = do
    putStrLn $ show result
    writeResults results


runReactors [] showEvents = return ()
runReactors reactors showEvents = do
    let (reactors', events') = initReactors reactors
    eventLoop showEvents reactors' events'