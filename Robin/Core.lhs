> module Robin.Core where

> import Data.Ratio

> import qualified Robin.Env as Env
> import Robin.Expr
> import Robin.Eval

Core
====

> robinHead env ienv (Pair expr Null) cc = do
>     eval env ienv expr (\x ->
>         assertPair ienv x (\(Pair a _) -> cc a))
> robinHead env ienv other cc = raise ienv (Pair (Symbol "illegal-arguments") other)

> robinTail env ienv (Pair expr Null) cc = do
>     eval env ienv expr (\x ->
>         assertPair ienv x (\(Pair _ b) -> cc b))
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
> numberP = predP isNumber

> robinSubtract env ienv (Pair xexpr (Pair yexpr Null)) cc = do
>     eval env ienv xexpr (\x ->
>         assertNumber ienv x (\(Number xv) ->
>             eval env ienv yexpr (\y ->
>                 assertNumber ienv y (\(Number yv) ->
>                     cc (Number (xv - yv))))))
> robinSubtract env ienv other cc = raise ienv (Pair (Symbol "illegal-arguments") other)

> robinDivide env ienv (Pair xexpr (Pair yexpr Null)) cc = do
>     eval env ienv xexpr (\x ->
>         assertNumber ienv x (\(Number xv) ->
>             eval env ienv yexpr (\y ->
>                 assertNumber ienv y (\(Number yv) ->
>                     if yv == (0%1) then
>                         raise ienv (Pair (Symbol "division-by-zero") x)
>                       else
>                         cc (Number (xv / yv))))))
> robinDivide env ienv other cc = raise ienv (Pair (Symbol "illegal-arguments") other)

> robinFloor env ienv (Pair expr Null) cc = do
>     eval env ienv expr (\x ->
>         assertNumber ienv x (\(Number xv) ->
>             cc $ Number (ratFloor xv % 1)))
> robinFloor env ienv other cc = raise ienv (Pair (Symbol "illegal-arguments") other)

> ratFloor x = numerator x `div` denominator x

> robinSign env ienv (Pair expr Null) cc = do
>     eval env ienv expr (\x ->
>         assertNumber ienv x (\(Number xv) ->
>             cc $ Number $ sign xv))
> robinSign env ienv other cc = raise ienv (Pair (Symbol "illegal-arguments") other)

> sign x = if x == 0 then 0 else if x < 0 then -1 else 1

> robinIf env ienv (Pair test (Pair texpr (Pair fexpr Null))) cc = do
>     eval env ienv test (\x ->
>         assertBoolean ienv x (\(Boolean b) ->
>             case b of
>                 True -> eval env ienv texpr cc
>                 False -> eval env ienv fexpr cc))
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
>         ("number?",  numberP),
>         ("equal?",   equalP),
>         ("subtract", robinSubtract),
>         ("divide",   robinDivide),
>         ("floor",    robinFloor),
>         ("sign",     robinSign),
>         ("macro",    macro),
>         ("eval",     robinEval),
>         ("if",       robinIf),
>         ("raise",    robinRaise)
>       ]
