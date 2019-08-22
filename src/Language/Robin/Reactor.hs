module Language.Robin.Reactor where

import qualified Data.Char as Char

import System.IO

import Language.Robin.Expr
import qualified Language.Robin.Env as Env
import Language.Robin.Eval

data Reactor = Reactor {
         env :: Expr,
         state :: Expr,
         body :: Expr   -- body takes three arguments: event-type event-payload state
     } deriving (Show, Eq)


initReactors :: [Reactor] -> IO [Reactor]
initReactors reactors = do
     reactors'' <- handleMany reactors (Symbol "init") (Number 0)
     return reactors''

eventLoop [] = return ()
eventLoop reactors = do
     stillOpen <- hIsOpen stdin
     case stillOpen of
         True -> do
             eof <- hIsEOF stdin
             case eof of
                 False -> cromulentEvent reactors
                 True  -> closeUpShop reactors
         False -> closeUpShop reactors

cromulentEvent reactors = do
    inpStr <- getLine
    let payload = List (map (\x -> Number (fromIntegral $ Char.ord x)) inpStr)
    reactors' <- handleMany reactors (Symbol "readln") payload
    eventLoop reactors'

closeUpShop reactors = do
    reactors' <- handleMany reactors (Symbol "eof") (Number 0)
    return ()

handleMany [] event payload = return []
handleMany (reactor:reactors) event payload = do
    retval <- return $ eval (IEnv stop) (env reactor) (List [(body reactor), event, payload, (state reactor)]) id
    maybeNewState <- handleRetVal retval (state reactor)
    rest <- handleMany reactors event payload
    case maybeNewState of
        Just state' -> do
            return (reactor{ state=state' }:rest)
        Nothing ->
            return rest

-- Returns Just the new state, or Nothing if the reactor bowed out.

handleRetVal retval state  =
    case retval of
        (List (state':responses)) -> do
            handleResponses state' responses
        _ -> do
            return (Just state)

-- Takes the state mainly so it can return Just state on success and Nothing if the reactor bowed out.
-- Also, after a close, it does not handle any more responses.  The reactor bowed out, after all.

handleResponses state [] = return (Just state)
handleResponses state (List [Symbol "writeln", payload]:responses) = do
    let List l = payload
    let s = map (\(Number x) -> Char.chr $ fromIntegral $ x) l
    hPutStrLn stdout s
    handleResponses state responses
handleResponses state (List [Symbol "close", payload]:responses) = do
    return Nothing
handleResponses state (response:responses) = do
    hPutStrLn stderr ("malformed response " ++ show response)
    handleResponses state responses
