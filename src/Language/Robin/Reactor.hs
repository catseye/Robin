module Language.Robin.Reactor where

import qualified Data.Char as Char
import Data.Int
import System.IO
import System.Random

import Language.Robin.Expr
import Language.Robin.Env (Env)
import Language.Robin.Eval

data Reactor = Reactor {
         rid :: Int32,
         env :: Env,
         state :: Expr,
         body :: Expr   -- body takes three arguments: event state
     } deriving (Show, Eq)

update :: Reactor -> Expr -> (Reactor, [Expr])
update reactor@Reactor{rid=rid, env=env, state=state, body=body} event =
    let
        -- If the reactor issued a 'stop' command, decorate that command
        -- with the rid of the reactor, so the event loop knows which
        -- reactor to stop.
        applyStop [] = []
        applyStop ((List [Symbol "stop", _]):commands) =
            (List [Symbol "stop", Number rid]:applyStop commands)
        applyStop (command:commands) =
            (command:applyStop commands)
    in
        case eval env (List [body, event, state]) id of
            (List (state':commands)) ->
                (reactor{ state=state' }, applyStop commands)
            expr ->
                -- make sure this event is ill-formed so that no reactors react to it
                -- TODO handle this in a more elegant way
                (reactor, [List [(Symbol "malformed-response"), (Symbol "malformed-response"), expr]])


updateMany :: [Reactor] -> Expr -> ([Reactor], [Expr])
updateMany [] event = ([], [])
updateMany (reactor:reactors) event =
    let
        (reactor', commands) = update reactor event
        (reactors', commands') = updateMany reactors event
    in
        ((reactor':reactors'), commands ++ commands')
