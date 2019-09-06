module Language.Robin.EventLoop where

import qualified Data.Char as Char
import Data.Int
import System.IO

import Language.Robin.Expr
import Language.Robin.Eval
import Language.Robin.Reactor

import Language.Robin.Facilities.LineTerminal (waitForLineTerminalEvent) -- unfortunate dependency


type Facility = Expr -> IO [Expr]


startEventLoop :: Bool -> [Reactor] -> [Facility] -> IO ()
startEventLoop showEvents reactors facilities = do
    let (reactors', events') = updateMany reactors (List [(Symbol "init"), (Number 0)])
    eventLoop showEvents reactors' facilities events'


eventLoop showEvents [] facilities events =
    -- No more reactors to react to things, so we can just stop.
    return ()

eventLoop showEvents reactors facilities (event@(List [Symbol "stop", Number reactorId]):events) = do
    showEvent showEvents event
    let reactors' = filter (\r -> rid r /= reactorId) reactors
    eventLoop showEvents reactors' facilities events

eventLoop showEvents reactors facilities (event@(List [eventType, eventPayload]):events) = do
    showEvent showEvents event
    newFacilityEvents <- runFacilityHandlers facilities event []
    let (reactors', newReactorEvents) = updateMany reactors event
    eventLoop showEvents reactors' facilities (events ++ newFacilityEvents ++ newReactorEvents)

eventLoop showEvents reactors facilities (event:events) = do
    showEvent showEvents event
    eventLoop showEvents reactors facilities events

eventLoop showEvents reactors facilities [] = do
    -- No events in queue, so wait for an event from the facilities that
    -- can produce them.  In our small case, this means the line-terminal.
    stillOpen <- hIsOpen stdin
    case stillOpen of
        True -> do
            eof <- hIsEOF stdin
            case eof of
                False -> do
                    event <- waitForLineTerminalEvent
                    eventLoop showEvents reactors facilities [event]
                True  -> return ()
        False -> return ()

runFacilityHandlers [] event acc = return acc
runFacilityHandlers (handler:handlers) event acc = do
    newEvents <- handler event
    runFacilityHandlers handlers event (acc ++ newEvents)


showEvent True event = hPutStrLn stderr ("*** " ++ show event)
showEvent False _ = return ()
