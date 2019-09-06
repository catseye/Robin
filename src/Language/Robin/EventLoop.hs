module Language.Robin.EventLoop where

import qualified Data.Char as Char
import Data.Int
import System.IO

import Language.Robin.Expr
import Language.Robin.Eval
import Language.Robin.Reactor
import Language.Robin.Facilities.LineTerminal
import Language.Robin.Facilities.RandomSource


startEventLoop :: Bool -> [Reactor] -> IO ()
startEventLoop showEvents reactors = do
    let (reactors', events') = updateMany reactors (List [(Symbol "init"), (Number 0)])
    eventLoop showEvents reactors' events'


eventLoop showEvents [] events =
    -- No more reactors to react to things, so we can just stop.
    return ()

eventLoop showEvents reactors (event@(List [Symbol "stop", Number reactorId]):events) = do
    showEvent showEvents event
    let reactors' = filter (\r -> rid r /= reactorId) reactors
    eventLoop showEvents reactors' events

eventLoop showEvents reactors (event@(List [eventType, eventPayload]):events) = do
    showEvent showEvents event
    newEvents1 <- handleLineTerminalEvent event
    newEvents2 <- handleRandomSourceEvent event
    let (reactors', newEvents3) = updateMany reactors event
    eventLoop showEvents reactors' (events ++ newEvents1 ++ newEvents2 ++ newEvents3)

eventLoop showEvents reactors (event:events) = do
    showEvent showEvents event
    eventLoop showEvents reactors events

eventLoop showEvents reactors [] = do
    -- No events in queue, so wait for an event from the facilities that
    -- can produce them.  In our small case, this means the line-terminal.
    stillOpen <- hIsOpen stdin
    case stillOpen of
        True -> do
            eof <- hIsEOF stdin
            case eof of
                False -> do
                    event <- waitForLineTerminalEvent
                    eventLoop showEvents reactors [event]
                True  -> return ()
        False -> return ()


showEvent True event = hPutStrLn stderr ("*** " ++ show event)
showEvent False _ = return ()
