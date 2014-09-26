> module Robin.Reactor where

> import qualified Data.Char as Char

> import System.IO

> import Robin.Expr
> import qualified Robin.Env as Env
> import Robin.Eval

> data Reactor = Reactor Expr Expr  Expr
>    deriving (Show, Eq)
> --                     env  state body
> -- body takes three arguments: event payload state

> initReactors :: [Reactor] -> IO [Reactor]
> initReactors reactors = do
>      reactors'' <- handleMany reactors (Symbol "init") (Number 0)
>      return reactors''

> eventLoop [] = return ()
> eventLoop reactors = do
>      stillOpen <- hIsOpen stdin
>      case stillOpen of
>          True -> do
>              eof <- hIsEOF stdin
>              case eof of
>                  False -> cromulentEvent reactors
>                  True  -> closeUpShop reactors
>          False -> closeUpShop reactors

> cromulentEvent reactors = do
>     inpStr <- getLine
>     let payload = List (map (\x -> Number (fromIntegral $ Char.ord x)) inpStr)
>     reactors' <- handleMany reactors (Symbol "readln") payload
>     eventLoop reactors'

> closeUpShop reactors = do
>     reactors' <- handleMany reactors (Symbol "eof") (Number 0)
>     return ()

> handleMany [] event payload = return []
> handleMany (reactor@(Reactor env state body):reactors) event payload = do
>     --print env
>     --print body
>     --print event
>     --print payload
>     retval <- eval (IEnv stop) env (List [body, event, payload, state]) (\x -> do return x)
>     maybeNewState <- handleRetVal retval state
>     rest <- handleMany reactors event payload
>     case maybeNewState of
>         Just state' -> do
>             return (Reactor env state' body:rest)
>         Nothing ->
>             return rest

Returns Just the new state, or Nothing if the reactor bowed out.

> handleRetVal retval state  =
>     case retval of
>         (List (state':responses)) -> do
>             handleResponses state' responses
>         _ -> do
>             return (Just state)

Takes the state mainly so it can return Just state on success and Nothing if the reactor bowed out.
Also, after a close, it does not handle any more responses.  The reactor bowed out, after all.

> handleResponses state [] = return (Just state)
> handleResponses state (List [Symbol "writeln", payload]:responses) = do
>     let List l = payload
>     let s = map (\(Number x) -> Char.chr $ fromIntegral $ x) l
>     hPutStrLn stdout s
>     handleResponses state responses
> handleResponses state (List [Symbol "close", payload]:responses) = do
>     return Nothing
> handleResponses state (response:responses) = do
>     hPutStrLn stderr ("malformed response " ++ show response)
>     handleResponses state responses