module Language.Robin.Facilities.LineTerminal where

import qualified Data.Char as Char
import System.IO

import Language.Robin.Expr
import Language.Robin.Facilities


waitForLineTerminalEvent :: WaitForEvents
waitForLineTerminalEvent = do
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


handleLineTerminalEvent :: FacilityHandler
handleLineTerminalEvent (List [Symbol "write", payload]) = do
    let List l = payload
    let s = map (\(Number x) -> Char.chr $ fromIntegral $ x) l
    hPutStr stdout s
    hFlush stdout
    return []
handleLineTerminalEvent (List [Symbol "writeln", payload]) = do
    let List l = payload
    let s = map (\(Number x) -> Char.chr $ fromIntegral $ x) l
    hPutStrLn stdout s
    hFlush stdout
    return []
handleLineTerminalEvent _ = return []
