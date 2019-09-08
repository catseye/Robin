module Language.Robin.Facilities.LineTerminal where

import qualified Data.Char as Char
import System.IO

import Language.Robin.Expr
import Language.Robin.Facilities


import Control.Concurrent (forkIO, myThreadId)
import Control.Concurrent.Chan


-- sketch
init :: IO FacilityHandler
init = do
    chan <- newChan
    threadId <- forkIO $ produceEvents chan
    return handleEvent


-- sketch
produceEvents :: Chan Event -> IO ()
produceEvents chan = do
    event <- waitForEvent
    writeChan chan event
    produceEvents chan


waitForEvent :: WaitForEvents
waitForEvent = do
    stillOpen <- hIsOpen stdin
    case stillOpen of
        True -> do
            eof <- hIsEOF stdin
            case eof of
                False -> do
                    inpStr <- getLine
                    let payload = List (map (\x -> Number (fromIntegral $ Char.ord x)) inpStr)
                    return $ Right [List [(Symbol "readln"), payload]]
                True  -> return $ Left "stop"
        False -> return $ Left "stop"


handleEvent :: FacilityHandler
handleEvent (List [Symbol "write", payload]) = do
    let List l = payload
    let s = map (\(Number x) -> Char.chr $ fromIntegral $ x) l
    hPutStr stdout s
    hFlush stdout
    return []
handleEvent (List [Symbol "writeln", payload]) = do
    let List l = payload
    let s = map (\(Number x) -> Char.chr $ fromIntegral $ x) l
    hPutStrLn stdout s
    hFlush stdout
    return []
handleEvent _ = return []
