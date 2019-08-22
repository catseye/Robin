module Language.Robin.Reactor where

import qualified Data.Char as Char

import System.IO

import Language.Robin.Expr
import Language.Robin.Eval

data Reactor = Reactor {
         env :: Expr,
         state :: Expr,
         body :: Expr   -- body takes three arguments: event-type event-payload state
     } deriving (Show, Eq)


update :: Reactor -> Expr -> Expr -> (Reactor, [Expr])
update reactor@Reactor{env=env, state=state, body=body} eventType payload =
    case eval (IEnv stop) env (List [body, eventType, payload, state]) id of
        (List (state':commands)) ->
            (reactor{ state=state' }, commands)
        _ ->
            -- in actuality, this is an error and we should log it etc.
            (reactor, [])


updateMany :: [Reactor] -> Expr -> Expr -> ([Reactor], [Expr])
updateMany [] eventType payload = ([], [])
updateMany (reactor:reactors) eventType payload =
    let
        (reactor', commands) = update reactor eventType payload
        (reactors', commands') = updateMany reactors eventType payload
    in
        ((reactor':reactors'), commands ++ commands')

--
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
--

initReactors reactors = updateMany reactors (Symbol "init") (Number 0)

eventLoop :: [Reactor] -> [Expr] -> IO ()
eventLoop reactors (event:events) = do
   handleLineTerminalEvent event
   let (reactors', newEvents) = updateMany reactors event (Number 0)
   eventLoop reactors' (events ++ newEvents)

eventLoop reactors [] = do
    -- No events in queue, so wait for an event from the facilities that
    -- can produce them.  In our small case, this means the line-terminal.
    stillOpen <- hIsOpen stdin
    case stillOpen of
        True -> do
            eof <- hIsEOF stdin
            case eof of
                False -> do
                    event <- waitForLineTerminalEvent
                    eventLoop reactors [event]
                True  -> return ()
        False -> return ()

waitForLineTerminalEvent = do
    inpStr <- getLine
    let payload = List (map (\x -> Number (fromIntegral $ Char.ord x)) inpStr)
    return (Symbol "readln") -- payload?

handleLineTerminalEvent (List [Symbol "writeln", payload]) = do
    let List l = payload
    let s = map (\(Number x) -> Char.chr $ fromIntegral $ x) l
    hPutStrLn stdout s
handleLineTerminalEvent _ = return ()
