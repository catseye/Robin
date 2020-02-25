module Language.Robin.EventLoop where

import Prelude (return, show, filter, (/=), (++), ($), Bool(True, False), Either(Left, Right))

import qualified Data.Char as Char
import Data.Int
import System.IO

import Language.Robin.Expr
import Language.Robin.Eval
import Language.Robin.Reactor
import Language.Robin.Facilities


eventLoop :: Bool -> [FacilityHandler] -> WaitForEvents -> [Reactor] -> IO ()
eventLoop showEvents facilities waitForEvents reactors = do
    let (reactors', events') = updateMany reactors (List [(Symbol "init"), (Number 0)])
    e reactors' events'
    where
        e [] events =
            -- No more reactors to react to things.  We can just stop.
            return ()

        e reactors (event@(List [Symbol "stop", Number reactorId]):events) = do
            -- A reactor requested to stop.  We remove it from our set.
            showEvent event
            let reactors' = filter (\r -> rid r /= reactorId) reactors
            e reactors' events

        e reactors (event@(List [eventType, eventPayload]):events) = do
            -- An event on the queue.  Allow all facilities and reactors to handle it.
            showEvent event
            newFacilityEvents <- runFacilityHandlers facilities event
            let (reactors', newReactorEvents) = updateMany reactors event
            e reactors' (events ++ newFacilityEvents ++ newReactorEvents)

        e reactors (event:events) = do
            -- Ill-formed event in queue.  Just discard it.
            showEvent event
            e reactors events

        e reactors [] = do
            -- Event queue is empty.  Wait for new events to arrive.
            result <- waitForEvents
            case result of
                Left err -> return ()
                Right events -> e reactors events

        runFacilityHandlers [] event = return []
        runFacilityHandlers (handler:handlers) event = do
            newEvents <- handler event
            rest <- runFacilityHandlers handlers event
            return $ newEvents ++ rest

        showEvent event = case showEvents of
            True -> hPutStrLn stderr ("*** " ++ show event)
            False -> return ()
