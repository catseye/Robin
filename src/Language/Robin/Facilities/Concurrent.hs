module Language.Robin.Facilities.Concurrent where

import Control.Concurrent (ThreadId, forkIO)
import Control.Concurrent.Chan

import Language.Robin.Facilities


orchestrate :: [IO Facility] -> IO ([FacilityHandler], WaitForEvents)
orchestrate facilities = do
    masterChan <- newChan
    facilities <- initEach facilities masterChan []
    return $ ([handler f | f <- facilities], globalWaitForEvents masterChan)
    where
        initEach [] chan acc = return acc
        initEach (syncInit:syncInits) chan acc = do
            facility <- syncInit
            threadId <- forkIO $ produceEvents chan (waiter facility)
            initEach syncInits chan (facility:acc)
        
        globalWaitForEvents chan = do
            message <- readChan chan
            return message

        produceEvents :: Chan Event -> WaitForEvents -> IO ()
        produceEvents chan waitForEvent = do
            event <- waitForEvent
            writeChan chan event
            produceEvents chan waitForEvent
