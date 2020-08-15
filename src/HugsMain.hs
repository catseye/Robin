module Main where

import System.Environment
import System.Exit

import Language.Robin.CmdLine

import Language.Robin.EventLoop (eventLoop)
import Language.Robin.Facilities (handler, waiter)
import qualified Language.Robin.Facilities.LineTerminal as LineTerminal
import qualified Language.Robin.Facilities.RandomSource as RandomSource


main = do
    args <- getArgs
    case args of
        [] -> do
            abortWithUsage
        _ -> do
            let (args', env', showEvents) = processFlags args
            (_, reactors, results) <- processArgs args' env'
            writeResults $ reverse results
            lineTerminal <- LineTerminal.init
            randomSource <- RandomSource.init
            let handlers = [handler lineTerminal, handler randomSource]
            let waitForEvents = waiter lineTerminal
            eventLoop showEvents handlers waitForEvents reactors
            exitWith ExitSuccess
