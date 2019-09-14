module Language.Robin.Reactor where

import qualified Data.Char as Char
import Data.Int
import System.IO
import System.Random

import qualified Language.Robin.Env as Env
import Language.Robin.Env (Env)
import Language.Robin.Expr
import Language.Robin.Eval

data Reactor = Reactor {
         rid :: Int32,
         env :: Env Expr,
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
