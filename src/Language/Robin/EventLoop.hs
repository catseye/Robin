module Language.Robin.EventLoop where

import qualified Data.Char as Char
import Data.Int
import System.IO

import Language.Robin.Expr
import Language.Robin.Eval
import Language.Robin.Reactor


type Facility = Expr -> IO [Expr]
type WaitForEvents = IO (Either String [Expr])


startEventLoop :: Bool -> [Reactor] -> [Facility] -> WaitForEvents -> IO ()
startEventLoop showEvents reactors facilities waitForEvents = do
    let (reactors', events') = updateMany reactors (List [(Symbol "init"), (Number 0)])
    eventLoop showEvents reactors' facilities waitForEvents events'


eventLoop :: Bool -> [Reactor] -> [Facility] -> WaitForEvents -> [Expr] -> IO ()
eventLoop showEvents [] facilities waitForEvents events =
    -- No more reactors to react to things, so we can just stop.
    return ()

eventLoop showEvents reactors facilities waitForEvents (event@(List [Symbol "stop", Number reactorId]):events) = do
    showEvent showEvents event
    let reactors' = filter (\r -> rid r /= reactorId) reactors
    eventLoop showEvents reactors' facilities waitForEvents events

eventLoop showEvents reactors facilities waitForEvents (event@(List [eventType, eventPayload]):events) = do
    showEvent showEvents event
    newFacilityEvents <- runFacilityHandlers facilities event []
    let (reactors', newReactorEvents) = updateMany reactors event
    eventLoop showEvents reactors' facilities waitForEvents (events ++ newFacilityEvents ++ newReactorEvents)
    where
        runFacilityHandlers [] event acc = return acc
        runFacilityHandlers (handler:handlers) event acc = do
            newEvents <- handler event
            runFacilityHandlers handlers event (acc ++ newEvents)


eventLoop showEvents reactors facilities waitForEvents (event:events) = do
    showEvent showEvents event
    eventLoop showEvents reactors facilities waitForEvents events

eventLoop showEvents reactors facilities waitForEvents [] = do
    -- event queue is empty.  wait for new events to arrive.
    result <- waitForEvents
    case result of
        Left err -> return ()
        Right newEvents -> eventLoop showEvents reactors facilities waitForEvents newEvents


showEvent True event = hPutStrLn stderr ("*** " ++ show event)
showEvent False _ = return ()
