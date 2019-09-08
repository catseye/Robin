module Language.Robin.Facilities where

import Control.Concurrent (ThreadId)
import Control.Concurrent.Chan

import Language.Robin.Expr

type Event = Either String [Expr]
type FacilityHandler = Expr -> IO [Expr]
type WaitForEvents = IO Event

data Facility = Facility {
                  threadId :: Maybe ThreadId,
                  handler :: FacilityHandler,
                  waiter :: WaitForEvents
                }

nullWaiter :: WaitForEvents
nullWaiter = return $ Right []

orchestrate :: [Chan Event -> IO Facility] -> IO ([FacilityHandler], WaitForEvents)
orchestrate facilities = do
    masterChan <- newChan
    facilities <- initEach facilities masterChan []
    return $ ([handler f | f <- facilities], waitForEvents masterChan)

initEach [] chan acc = return acc
initEach (init:inits) chan acc = do
    facility <- init chan
    initEach inits chan (facility:acc)

waitForEvents chan = do
    message <- readChan chan
    return message
