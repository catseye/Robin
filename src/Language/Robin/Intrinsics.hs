module Language.Robin.Intrinsics where

import Language.Robin.Expr
import Language.Robin.Eval


robinHead :: Evaluable
robinHead i env (List [expr]) cc =
    eval i env expr (\x ->
        assertList i x (\val ->
            case val of
                List (a:_) -> cc a
                other -> raise i (errMsg "expected-list" other)))
robinHead i env other cc = raise i (errMsg "illegal-arguments" other)

robinTail :: Evaluable
robinTail i env (List [expr]) cc =
    eval i env expr (\x ->
        assertList i x (\val ->
            case val of
                List (_:b) -> cc (List b)
                other -> raise i (errMsg "expected-list" other)))
robinTail i env other cc = raise i (errMsg "illegal-arguments" other)

robinPrepend :: Evaluable
robinPrepend i env (List [e1, e2]) cc =
    eval i env e1 (\x1 -> eval i env e2 (\val ->
            case val of
                List x2 -> cc $ List (x1:x2)
                other -> raise i (errMsg "expected-list" other)))
robinPrepend i env other cc = raise i (errMsg "illegal-arguments" other)

equalP :: Evaluable
equalP i env (List [e1, e2]) cc =
    eval i env e1 (\x1 -> eval i env e2 (\x2 -> cc $ Boolean (x1 == x2)))
equalP i env other cc = raise i (errMsg "illegal-arguments" other)

predP pred i env (List [expr]) cc =
    eval i env expr (\x -> cc $ Boolean $ pred x)
predP pred i env other cc = raise i (errMsg "illegal-arguments" other)

symbolP = predP isSymbol
listP = predP isList
macroP = predP isMacro
numberP = predP isNumber

robinSubtract :: Evaluable
robinSubtract i env (List [xexpr, yexpr]) cc =
    eval i env xexpr (\x ->
        assertNumber i x (\(Number xv) ->
            eval i env yexpr (\y ->
                assertNumber i y (\(Number yv) ->
                    cc (Number (xv - yv))))))
robinSubtract i env other cc = raise i (errMsg "illegal-arguments" other)

robinSign :: Evaluable
robinSign i env (List [expr]) cc =
    eval i env expr (\x ->
        assertNumber i x (\(Number xv) ->
            cc $ Number $ sign xv))
    where
        sign x = if x == 0 then 0 else if x < 0 then -1 else 1
robinSign i env other cc = raise i (errMsg "illegal-arguments" other)

robinIf :: Evaluable
robinIf i env (List [test, texpr, fexpr]) cc =
    eval i env test (\x ->
        assertBoolean i x (\(Boolean b) ->
            case b of
                True -> eval i env texpr cc
                False -> eval i env fexpr cc))
robinIf i env other cc = raise i (errMsg "illegal-arguments" other)

robinEval :: Evaluable
robinEval i env (List [envlist, form]) cc =
    eval i env envlist (\newEnvVal ->
        eval i env form (\body ->
            eval i newEnvVal body cc))
robinEval i env other cc = raise i (errMsg "illegal-arguments" other)

robinMacro :: Evaluable
robinMacro i env (List [args@(List [(Symbol selfS), (Symbol argsS), (Symbol envS)]), body]) cc =
    cc $ Macro env args body
robinMacro i env other cc = raise i (errMsg "illegal-arguments" other)

robinRaise :: Evaluable
robinRaise i env (List [expr]) cc =
    eval i env expr (\v -> raise i v)
robinRaise i env other cc = raise i (errMsg "illegal-arguments" other)

robinCatch :: Evaluable
robinCatch i env (List [(Symbol s), handler, body]) cc =
    let
        handlerContinuation = (\errvalue ->
            eval i (insert s errvalue env) handler cc)
        i' = setExceptionHandler handlerContinuation i
    in
        eval i' env body cc
robinCatch i env other cc = raise i (errMsg "illegal-arguments" other)

robinIntrinsics :: Env
robinIntrinsics = fromList $ map (\(name,bif) -> (name, Intrinsic name bif))
      [
        ("head",     robinHead),
        ("tail",     robinTail),
        ("prepend",  robinPrepend),
        ("list?",    listP),
        ("symbol?",  symbolP),
        ("macro?",   macroP),
        ("number?",  numberP),
        ("equal?",   equalP),
        ("subtract", robinSubtract),
        ("sign",     robinSign),
        ("macro",    robinMacro),
        ("eval",     robinEval),
        ("if",       robinIf),
        ("raise",    robinRaise),
        ("catch",    robinCatch)
      ]
