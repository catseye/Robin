> module Robin.Core where

> import Data.Ratio

> import qualified Robin.Env as Env
> import Robin.Expr
> import Robin.Eval

Core
====

> robinHead env ienv (List [expr]) cc = do
>     eval env ienv expr (\x ->
>         assertList ienv x (\val ->
>             case val of
>                 List (a:_) -> cc a
>                 other -> raise ienv (errMsg "expected-list" other)))
> robinHead env ienv other cc = raise ienv (errMsg "illegal-arguments" other)

> robinTail env ienv (List [expr]) cc = do
>     eval env ienv expr (\x ->
>         assertList ienv x (\val ->
>             case val of
>                 List (_:b) -> cc (List b)
>                 other -> raise ienv (errMsg "expected-list" other)))
> robinTail env ienv other cc = raise ienv (errMsg "illegal-arguments" other)

> robinPair env ienv (List [e1, e2]) cc = do
>     eval env ienv e1 (\x1 -> eval env ienv e2 (\(List x2) -> cc $ List (x1:x2)))
> robinPair env ienv other cc = raise ienv (errMsg "illegal-arguments" other)

> equalP env ienv (List [e1, e2]) cc = do
>     eval env ienv e1 (\x1 -> eval env ienv e2 (\x2 -> cc $ Boolean (x1 == x2)))
> equalP env ienv other cc = raise ienv (errMsg "illegal-arguments" other)

> predP pred env ienv (List [expr]) cc = do
>     eval env ienv expr (\x -> cc $ Boolean $ pred $ stripMetadata x)
> predP pred env ienv other cc = raise ienv (errMsg "illegal-arguments" other)

> symbolP = predP isSymbol
> booleanP = predP isBoolean
> listP = predP isList
> macroP = predP isMacro
> numberP = predP isNumber

> robinSubtract env ienv (List [xexpr, yexpr]) cc = do
>     eval env ienv xexpr (\x ->
>         assertNumber ienv x (\(Number xv) ->
>             eval env ienv yexpr (\y ->
>                 assertNumber ienv y (\(Number yv) ->
>                     cc (Number (xv - yv))))))
> robinSubtract env ienv other cc = raise ienv (errMsg "illegal-arguments" other)

> robinDivide env ienv (List [xexpr, yexpr]) cc = do
>     eval env ienv xexpr (\x ->
>         assertNumber ienv x (\(Number xv) ->
>             eval env ienv yexpr (\y ->
>                 assertNumber ienv y (\(Number yv) ->
>                     if yv == (0%1) then
>                         raise ienv (errMsg "division-by-zero" x)
>                       else
>                         cc (Number (xv / yv))))))
> robinDivide env ienv other cc = raise ienv (errMsg "illegal-arguments" other)

> robinFloor env ienv (List [expr]) cc = do
>     eval env ienv expr (\x ->
>         assertNumber ienv x (\(Number xv) ->
>             cc $ Number (ratFloor xv % 1)))
> robinFloor env ienv other cc = raise ienv (errMsg "illegal-arguments" other)

> ratFloor x = numerator x `div` denominator x

> robinSign env ienv (List [expr]) cc = do
>     eval env ienv expr (\x ->
>         assertNumber ienv x (\(Number xv) ->
>             cc $ Number $ sign xv))
> robinSign env ienv other cc = raise ienv (errMsg "illegal-arguments" other)

> sign x = if x == 0 then 0 else if x < 0 then -1 else 1

> robinIf env ienv (List [test, texpr, fexpr]) cc = do
>     eval env ienv test (\x ->
>         assertBoolean ienv x (\(Boolean b) ->
>             case b of
>                 True -> eval env ienv texpr cc
>                 False -> eval env ienv fexpr cc))
> robinIf env ienv other cc = raise ienv (errMsg "illegal-arguments" other)

> robinEval env ienv (List [envlist, form]) cc = do
>     eval env ienv envlist (\newEnv ->
>         eval env ienv form (\body -> do
>             eval newEnv ienv body cc))
> robinEval env ienv other cc = raise ienv (errMsg "illegal-arguments" other)

> macro env ienv (List [args@(List [(Symbol selfS), (Symbol argsS), (Symbol envS)]), body]) cc = do
>     cc $ Macro env args body
> macro env ienv other cc = raise ienv (errMsg "illegal-arguments" other)

> robinRaise env ienv (List [expr]) cc =
>     eval env ienv expr (\v -> raise ienv v)
> robinRaise env ienv other cc = raise ienv (errMsg "illegal-arguments" other)

> robinWith env ienv (List [metadataExpr, expr]) cc =
>     eval env ienv metadataExpr (\metadata ->
>         eval env ienv expr (\value ->
>             cc $ Metadata metadata value))
> robinWith env ienv other cc = raise ienv (errMsg "illegal-arguments" other)

> hasP env ienv (List [metadataExpr, expr]) cc =
>     eval env ienv metadataExpr (\metadata ->
>         eval env ienv expr (\value ->
>             cc $ Boolean $ hasMetadata metadata value))
> hasP env ienv other cc = raise ienv (errMsg "illegal-arguments" other)

Module Definition
-----------------

> moduleId = ("core", 0, 1)

> moduleDef :: IO Expr
> moduleDef = do
>     return $ Env.fromList $ map (\(name,bif) -> (name, Builtin name bif))
>       [
>         ("head",     robinHead),
>         ("tail",     robinTail),
>         ("pair",     robinPair),
>         ("list?",    listP),
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
>         ("with",     robinWith),
>         ("has?",     hasP),
>         ("raise",    robinRaise)
>       ]
