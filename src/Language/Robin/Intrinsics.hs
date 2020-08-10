module Language.Robin.Intrinsics where

import Language.Robin.Expr
import Language.Robin.Env (Env, fromList, insert)
import Language.Robin.Eval


head_ :: Evaluable
head_ env (List [expr]) cc =
    evalToList cc env expr (\val ->
       case val of
           List (a:_) -> cc a
           other -> errMsg cc "expected-list" other)  -- FIXME: should really be "expected-nonempty-list"
head_ env other cc = errMsg cc "illegal-arguments" other

tail_ :: Evaluable
tail_ env (List [expr]) cc =
    evalToList cc env expr (\val ->
        case val of
            List (_:b) -> cc (List b)
            other -> errMsg cc "expected-list" other)
tail_ env other cc = errMsg cc "illegal-arguments" other

prepend :: Evaluable
prepend env (List [e1, e2]) cc =
    evalB cc env e1 (\x1 ->
        evalToList cc env e2 (\(List x2) -> cc $ List (x1:x2)))
prepend env other cc = errMsg cc "illegal-arguments" other

equalP :: Evaluable
equalP env (List [e1, e2]) cc =
    evalB cc env e1 (\x1 -> evalB cc env e2 (\x2 -> cc $ Boolean (x1 == x2)))
equalP env other cc = errMsg cc "illegal-arguments" other

predP pred env (List [expr]) cc =
    evalB cc env expr (\x -> cc $ Boolean $ pred x)
predP pred env other cc = errMsg cc "illegal-arguments" other

symbolP = predP isSymbol
listP = predP isList
operatorP = predP isOperator
numberP = predP isNumber

subtract_ :: Evaluable
subtract_ = evalTwoNumbers (\x y cc -> cc $ Number (x - y))

sign :: Evaluable
sign env (List [expr]) cc =
    let
        sgn x = if x == 0 then 0 else if x < 0 then -1 else 1
    in
        evalToNumber cc env expr (\(Number xv) ->
            cc $ Number $ sgn xv)
sign env other cc = errMsg cc "illegal-arguments" other

if_ :: Evaluable
if_ env (List [testExpr, trueExpr, falseExpr]) cc =
    evalToBoolean cc env testExpr (\(Boolean b) ->
        if b then eval env trueExpr cc else eval env falseExpr cc)
if_ env other cc = errMsg cc "illegal-arguments" other

eval_ :: Evaluable
eval_ env (List [envlist, form]) cc =
    evalB cc env envlist (\newEnvVal ->
        evalB cc env form (\body ->
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
