> module Robin.Console where

> import Control.Concurrent (myThreadId)

> import UI.HSCurses.Curses hiding (Pair)

> import Robin.Chan
> import Robin.Expr
> import qualified Robin.Env as Env
> import Robin.Parser

> import Robin.Core (ratFloor)
> import Robin.Concurrency (spawn, respond)

Console
=======

A rudimentary virtual console module for Robin, based loosely on
`Console::Virtual`.

The virtual console output device accepts messages, and alters the
state of the virtual console based on those messages.

> outputHandler :: Chan Expr -> IO ()

> outputHandler chan = respond chan [
>         ("activate", \(x, y) sender payload -> do
>             initCurses
>             screen <- initScr
>             cBreak True
>             echo False
>             keypad stdScr True
>             return ((x, y), Symbol "ok")),
>         ("deactivate", \(x, y) sender payload -> do
>             keypad stdScr False
>             echo True
>             cBreak False
>             endWin
>             return ((x, y), Symbol "ok")),
>         ("display", \(x, y) sender payload -> do
>             let str = show payload
>             mvWAddStr stdScr y x str
>             return ((x + length str, y), Symbol "ok")),
>         ("clear-screen", \(x, y) sender payload -> do
>             wMove stdScr 0 0
>             wclear stdScr
>             refresh
>             return ((0, 0), Symbol "ok")),
>         ("clear-eol", \(x, y) sender payload -> do
>             wMove stdScr y x
>             clrToEol
>             return ((x, y), Symbol "ok")),
>         ("position", \(x, y) sender (List [Number xr, Number yr])  -> do
>             let x' = fromIntegral (ratFloor xr) :: Int
>             let y' = fromIntegral (ratFloor yr) :: Int
>             return ((x', y'), Symbol "ok")),
>         ("update", \(x, y) sender payload -> do
>             wMove stdScr y x
>             refresh
>             return ((x, y), Symbol "ok"))
>     ] (0, 0)

Module Definition
-----------------

> moduleId = ("console", 0, 1)

> moduleDef :: IO Expr
> moduleDef = do
>     consoleOutputPid <- spawn outputHandler
>     return $ Env.fromList (
>       [
>         ("console", consoleOutputPid)
>       ])
