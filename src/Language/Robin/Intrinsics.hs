module Language.Robin.Intrinsics where

import Language.Robin.Expr
import Language.Robin.Eval


robinHead :: Evaluable
robinHead env (List [expr]) cc =
    eval env expr (\x ->
        assertList env x (\val ->
            case val of
                List (a:_) -> cc a
                other -> errMsg "expected-list" other))
robinHead env other cc = errMsg "illegal-arguments" other

robinTail :: Evaluable
robinTail env (List [expr]) cc =
    eval env expr (\x ->
        assertList env x (\val ->
            case val of
                List (_:b) -> cc (List b)
                other -> errMsg "expected-list" other))
robinTail env other cc = errMsg "illegal-arguments" other

robinPrepend :: Evaluable
robinPrepend env (List [e1, e2]) cc =
    eval env e1 (\x1 -> eval env e2 (\val ->
            case val of
                List x2 -> cc $ List (x1:x2)
                other -> errMsg "expected-list" other))
robinPrepend env other cc = errMsg "illegal-arguments" other

equalP :: Evaluable
equalP env (List [e1, e2]) cc =
    eval env e1 (\x1 -> eval env e2 (\x2 -> cc $ Boolean (x1 == x2)))
equalP env other cc = errMsg "illegal-arguments" other

predP pred env (List [expr]) cc =
    eval env expr (\x -> cc $ Boolean $ pred x)
predP pred env other cc = errMsg "illegal-arguments" other

symbolP = predP isSymbol
listP = predP isList
macroP = predP isMacro
numberP = predP isNumber

robinSubtract :: Evaluable
robinSubtract env (List [xexpr, yexpr]) cc =
    eval env xexpr (\x ->
        assertNumber env x (\(Number xv) ->
            eval env yexpr (\y ->
                assertNumber env y (\(Number yv) ->
                    cc (Number (xv - yv))))))
robinSubtract env other cc = errMsg "illegal-arguments" other

robinSign :: Evaluable
robinSign env (List [expr]) cc =
    eval env expr (\x ->
        assertNumber env x (\(Number xv) ->
            cc $ Number $ sign xv))
    where
        sign x = if x == 0 then 0 else if x < 0 then -1 else 1
robinSign env other cc = errMsg "illegal-arguments" other

robinIf :: Evaluable
robinIf env (List [test, texpr, fexpr]) cc =
    eval env test (\x ->
        assertBoolean env x (\(Boolean b) ->
            case b of
                True -> eval env texpr cc
                False -> eval env fexpr cc))
robinIf env other cc = errMsg "illegal-arguments" other

robinEval :: Evaluable
robinEval env (List [envlist, form]) cc =
    eval env envlist (\newEnvVal ->
        eval env form (\body ->
            eval newEnvVal body cc))
robinEval env other cc = errMsg "illegal-arguments" other

robinMacro :: Evaluable
robinMacro env (List [args@(List [(Symbol selfS), (Symbol argsS), (Symbol envS)]), body]) k =
    k $ Macro env args body
robinMacro env other cc = errMsg "illegal-arguments" other

robinRaise :: Evaluable
robinRaise env (List [expr]) cc =
    eval env expr (\v -> cc $ Error v)
robinRaise env other cc = errMsg "illegal-arguments" other

robinCatch :: Evaluable
robinCatch env (List [expr, (Symbol s), errexpr, (Symbol v), okexpr]) cc =
    eval env expr (\result ->
        case result of
            e@(Error contents) ->
                eval (insert s contents env) errexpr cc
            other ->
                eval (insert v other env) okexpr cc)
robinCatch env other cc = errMsg "illegal-arguments" other

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
