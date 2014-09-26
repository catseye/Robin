> module Robin.Intrinsics where

> import qualified Robin.Env as Env
> import Robin.Expr
> import Robin.Eval

Intrinsics
==========

> robinHead i env (List [expr]) cc = do
>     eval i env expr (\x ->
>         assertList i x (\val ->
>             case val of
>                 List (a:_) -> cc a
>                 other -> raise i (errMsg "expected-list" other)))

> robinTail i env (List [expr]) cc = do
>     eval i env expr (\x ->
>         assertList i x (\val ->
>             case val of
>                 List (_:b) -> cc (List b)
>                 other -> raise i (errMsg "expected-list" other)))

> robinPrepend i env (List [e1, e2]) cc = do
>     eval i env e1 (\x1 -> eval i env e2 (\val ->
>             case val of
>                 List x2 -> cc $ List (x1:x2)
>                 other -> raise i (errMsg "expected-list" other)))

> equalP i env (List [e1, e2]) cc = do
>     eval i env e1 (\x1 -> eval i env e2 (\x2 -> cc $ Boolean (x1 == x2)))

> predP pred i env (List [expr]) cc = do
>     eval i env expr (\x -> cc $ Boolean $ pred x)

> symbolP = predP isSymbol
> listP = predP isList
> macroP = predP isMacro
> numberP = predP isNumber

> robinSubtract i env (List [xexpr, yexpr]) cc = do
>     eval i env xexpr (\x ->
>         assertNumber i x (\(Number xv) ->
>             eval i env yexpr (\y ->
>                 assertNumber i y (\(Number yv) ->
>                     cc (Number (xv - yv))))))

> robinSign i env (List [expr]) cc = do
>     eval i env expr (\x ->
>         assertNumber i x (\(Number xv) ->
>             cc $ Number $ sign xv))

> sign x = if x == 0 then 0 else if x < 0 then -1 else 1

> robinIf i env (List [test, texpr, fexpr]) cc = do
>     eval i env test (\x ->
>         assertBoolean i x (\(Boolean b) ->
>             case b of
>                 True -> eval i env texpr cc
>                 False -> eval i env fexpr cc))

> robinEval i env (List [envlist, form]) cc = do
>     eval i env envlist (\newEnv ->
>         eval i env form (\body -> do
>             eval i newEnv body cc))

> robinMacro i env (List [args@(List [(Symbol selfS), (Symbol argsS), (Symbol envS)]), body]) cc = do
>     cc $ Macro env args body

> robinRaise i env (List [expr]) cc =
>     eval i env expr (\v -> raise i v)

> robinCatch i env (List [id@(Symbol _), handler, body]) cc =
>     let
>         handlerContinuation = (\errvalue ->
>             eval i (Env.insert id errvalue env) handler cc)
>         i' = setExceptionHandler handlerContinuation i
>     in
>         eval i' env body cc

> robinIntrinsics = Env.fromList $ map (\(name,bif) -> (name, Intrinsic name bif))
>       [
>         ("@head",     robinHead),
>         ("@tail",     robinTail),
>         ("@prepend",  robinPrepend),
>         ("@list?",    listP),
>         ("@symbol?",  symbolP),
>         ("@macro?",   macroP),
>         ("@number?",  numberP),
>         ("@equal?",   equalP),
>         ("@subtract", robinSubtract),
>         ("@sign",     robinSign),
>         ("@macro",    robinMacro),
>         ("@eval",     robinEval),
>         ("@if",       robinIf),
>         ("@raise",    robinRaise),
>         ("@catch",    robinCatch)
>       ]
