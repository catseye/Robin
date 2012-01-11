> module Robin.Core where

> import qualified Robin.Env as Env
> import Robin.Expr
> import Robin.Eval

Core
====

> robinHead env ienv (Pair expr Null) cc = do
>     eval env ienv expr (\x ->
>         case x of
>             (Pair a _) -> cc $ a
>             other      -> raise ienv (Pair (Symbol "expected-pair") other))
> robinHead env ienv other cc = raise ienv (Pair (Symbol "illegal-arguments") other)

> robinTail env ienv (Pair expr Null) cc = do
>     eval env ienv expr (\x ->
>         case x of
>             (Pair _ b) -> cc $ b
>             other      -> raise ienv (Pair (Symbol "expected-pair") other))
> robinTail env ienv other cc = raise ienv (Pair (Symbol "illegal-arguments") other)

> robinPair env ienv (Pair e1 (Pair e2 Null)) cc = do
>     eval env ienv e1 (\x1 -> eval env ienv e2 (\x2 -> cc $ Pair x1 x2))
> robinPair env ienv other cc = raise ienv (Pair (Symbol "illegal-arguments") other)

> equalP env ienv (Pair e1 (Pair e2 Null)) cc = do
>     eval env ienv e1 (\x1 -> eval env ienv e2 (\x2 -> cc $ Boolean (x1 == x2)))
> equalP env ienv other cc = raise ienv (Pair (Symbol "illegal-arguments") other)

> predP pred env ienv (Pair e Null) cc = do
>     eval env ienv e (\x -> cc $ Boolean (pred x))
> predP pred env ienv other cc = raise ienv (Pair (Symbol "illegal-arguments") other)

> symbolP = predP isSymbol
> booleanP = predP isBoolean
> pairP = predP isPair
> macroP = predP isMacro

> robinIf env ienv (Pair test (Pair texpr (Pair fexpr Null))) cc = do
>     eval env ienv test (\x ->
>         case x of
>             Boolean True -> eval env ienv texpr cc
>             Boolean False -> eval env ienv fexpr cc
>             other ->
>                 raise ienv (Pair (Symbol "expected-boolean") other))
> robinIf env ienv other cc = raise ienv (Pair (Symbol "illegal-arguments") other)

> robinEval env ienv (Pair envlist (Pair form Null)) cc = do
>     eval env ienv envlist (\newEnv ->
>         eval env ienv form (\body -> do
>             eval newEnv ienv body cc))
> robinEval env ienv other cc = raise ienv (Pair (Symbol "illegal-arguments") other)

> macro env ienv (Pair args@(Pair (Symbol selfS) (Pair (Symbol argsS) (Pair (Symbol envS) Null))) (Pair body Null)) cc = do
>     cc $ Macro env args body
> macro env ienv other cc = raise ienv (Pair (Symbol "illegal-arguments") other)

> robinRaise env ienv (Pair expr Null) cc =
>     eval env ienv expr (\v -> raise ienv v)

Module Definition
-----------------

> moduleCore :: IO Expr

> moduleCore = do
>     return $ Env.fromList $ map (\(name,bif) -> (name, Builtin name bif))
>       [
>         ("head",     robinHead),
>         ("tail",     robinTail),
>         ("pair",     robinPair),
>         ("pair?",    pairP),
>         ("symbol?",  symbolP),
>         ("boolean?", booleanP),
>         ("macro?",   macroP),
>         ("equal?",   equalP),
>         ("macro",    macro),
>         ("eval",     robinEval),
>         ("if",       robinIf),
>         ("raise",    robinRaise)
>       ]

