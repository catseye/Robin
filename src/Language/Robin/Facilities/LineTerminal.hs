module Language.Robin.Facilities.LineTerminal where

import qualified Data.Char as Char
import System.IO

import Language.Robin.Expr


waitForLineTerminalEvent = do
    inpStr <- getLine
    let payload = List (map (\x -> Number (fromIntegral $ Char.ord x)) inpStr)
    return $ List [(Symbol "readln"), payload]

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
