module Language.Robin.Reactor where

import qualified Data.Char as Char
import Data.Int
import System.IO
import System.Random

import Language.Robin.Expr
import Language.Robin.Eval

data Reactor = Reactor {
         rid :: Int32,
         env :: Expr,
         state :: Expr,
         body :: Expr   -- body takes three arguments: event state
     } deriving (Show, Eq)


update :: Reactor -> Expr -> (Reactor, [Expr])
update reactor@Reactor{rid=rid, env=env, state=state, body=body} event =
    case eval (IEnv catchException) env (List [body, event, state]) id of
        command@(List [(Symbol "uncaught-exception"), expr]) ->
            (reactor, [command])
        (List (state':commands)) ->
            (reactor{ state=state' }, applyStop commands)
        expr ->
            (reactor, [List [(Symbol "malformed-response"), expr]])
    where
        catchException expr = List [(Symbol "uncaught-exception"), expr]

        -- If the reactor issued a 'stop' command, decorate that command
        -- with the rid of the reactor, so the event loop knows which
        -- reactor to stop.
        applyStop [] = []
        applyStop ((List [Symbol "stop", _]):commands) =
            (List [Symbol "stop", Number rid]:applyStop commands)
        applyStop (command:commands) =
            (command:applyStop commands)


updateMany :: [Reactor] -> Expr -> ([Reactor], [Expr])
updateMany [] event = ([], [])
updateMany (reactor:reactors) event =
    let
        (reactor', commands) = update reactor event
        (reactors', commands') = updateMany reactors event
    in
        ((reactor':reactors'), commands ++ commands')

--
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
--

initReactors reactors = updateMany reactors (List [(Symbol "init"), (Number 0)])

showEvent True event = hPutStrLn stderr ("*** " ++ show event)
showEvent False _ = return ()

eventLoop :: Bool -> [Reactor] -> [Expr] -> IO ()

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

handleRandomSourceEvent (List [Symbol "obtain-random", payload]) = do
    -- FIXME should be: v <- randomRIO (minBound :: Int32, maxBound :: Int32)
    v <- randomRIO (0, 1000)
    return $ [List [Symbol "random", Number v]]
handleRandomSourceEvent _ = return []
