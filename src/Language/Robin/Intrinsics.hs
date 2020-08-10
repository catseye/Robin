module Language.Robin.Intrinsics where

import Language.Robin.Expr
import Language.Robin.Env (Env, fromList, insert)
import Language.Robin.Eval


head_ :: Evaluable
head_ env (List [expr]) cc =
    eval env expr (\x ->
        assertList (cc) env x (\val ->
            case val of
                List (a:_) -> cc a
                other -> errMsg cc "expected-list" other))
head_ env other cc = errMsg cc "illegal-arguments" other

tail_ :: Evaluable
tail_ env (List [expr]) cc =
    eval env expr (\x ->
        assertList (cc) env x (\val ->
            case val of
                List (_:b) -> cc (List b)
                other -> errMsg cc "expected-list" other))
tail_ env other cc = errMsg cc "illegal-arguments" other

prepend :: Evaluable
prepend env (List [e1, e2]) cc =
    eval env e1 (\x1 ->
        case x1 of
            Abort _ -> cc x1
            _ -> eval env e2 (\val ->
                    case val of
                        List x2 -> cc $ List (x1:x2)
                        other -> errMsg cc "expected-list" other))
prepend env other cc = errMsg cc "illegal-arguments" other

equalP :: Evaluable
equalP env (List [e1, e2]) cc =
    eval env e1 (\x1 -> eval env e2 (\x2 -> cc $ Boolean (x1 == x2)))
equalP env other cc = errMsg cc "illegal-arguments" other

predP pred env (List [expr]) cc =
    eval env expr (\x -> cc $ Boolean $ pred x)
predP pred env other cc = errMsg cc "illegal-arguments" other

symbolP = predP isSymbol
listP = predP isList
operatorP = predP isOperator
numberP = predP isNumber

subtract_ :: Evaluable
subtract_ env (List [xexpr, yexpr]) cc =
    eval env xexpr (\x ->
        assertNumber cc env x (\(Number xv) ->
            eval env yexpr (\y ->
                assertNumber cc env y (\(Number yv) ->
                    cc (Number (xv - yv))))))
subtract_ env other cc = errMsg cc "illegal-arguments" other

sign :: Evaluable
sign env (List [expr]) cc =
    let
        sgn x = if x == 0 then 0 else if x < 0 then -1 else 1
    in
        eval env expr (\x ->
            assertNumber cc env x (\(Number xv) ->
                cc $ Number $ sgn xv))
sign env other cc = errMsg cc "illegal-arguments" other

if_ :: Evaluable
if_ env (List [test, texpr, fexpr]) cc =
    eval env test (\x ->
        assertBoolean cc env x (\(Boolean b) ->
            if b then eval env texpr cc else eval env fexpr cc))
if_ env other cc = errMsg cc "illegal-arguments" other

eval_ :: Evaluable
eval_ env (List [envlist, form]) cc =
    eval env envlist (\newEnvVal ->
        eval env form (\body ->
            eval newEnvVal body cc))
eval_ env other cc = errMsg cc "illegal-arguments" other

macro :: Evaluable
macro env (List [args@(List [(Symbol argsS), (Symbol envS)]), body]) cc =
    cc $ Operator "<operator>" $ makeMacro env args body
macro env other cc = errMsg cc "illegal-arguments" other

abort :: Evaluable
abort env (List [expr]) cc =
    eval env expr (\v -> cc $ Abort v)
abort env other cc = errMsg cc "illegal-arguments" other

recover :: Evaluable
recover env (List [expr, (Symbol okName), okExpr, (Symbol abortName), abortExpr]) cc =
    eval env expr (\result ->
        case result of
            e@(Abort contents) ->
                eval (insert abortName contents env) abortExpr cc
            other ->
                eval (insert okName other env) okExpr cc)
recover env other cc = errMsg cc "illegal-arguments" other

robinIntrinsics :: Env
robinIntrinsics = fromList $ map (\(name,bif) -> (name, Operator name bif))
      [
        ("head",     head_),
        ("tail",     tail_),
        ("prepend",  prepend),
        ("list?",    listP),
        ("symbol?",  symbolP),
        ("operator?",operatorP),
        ("number?",  numberP),
        ("equal?",   equalP),
        ("subtract", subtract_),
        ("sign",     sign),
        ("macro",    macro),
        ("eval",     eval_),
        ("if",       if_),
        ("abort",    abort),
        ("recover",  recover)
      ]
