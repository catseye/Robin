module Language.Robin.Intrinsics where

import Language.Robin.Expr
import Language.Robin.Env (Env, fromList, insert)
import Language.Robin.Eval


head_ :: Evaluable
head_ env (List [expr]) =
    case eval env expr of
        List (a:_) -> a
        other -> errMsg "expected-list" other
head_ env other = errMsg "illegal-arguments" other

tail_ :: Evaluable
tail_ env (List [expr]) =
    case eval env expr of
        List (_:b) -> (List b)
        other -> errMsg "expected-list" other
tail_ env other = errMsg "illegal-arguments" other

prepend :: Evaluable
prepend env (List [e1, e2]) =
    case (eval env e1, eval env e2) of
        ((Abort a), _) -> Abort a
        (_, (Abort a)) -> Abort a
        (x1, List x2)  -> List (x1:x2)
        (_, other)     -> errMsg "expected-list" other
prepend env other = errMsg "illegal-arguments" other

equalP :: Evaluable
equalP env (List [e1, e2]) =
    let (x1, x2) = (eval env e1, eval env e2) in Boolean (x1 == x2)
equalP env other = errMsg "illegal-arguments" other

predP pred env (List [expr]) =
    Boolean $ pred (eval env expr)
predP pred env other = errMsg "illegal-arguments" other

symbolP = predP isSymbol
listP = predP isList
operatorP = predP isOperator
numberP = predP isNumber

subtract_ :: Evaluable
subtract_ env (List [xexpr, yexpr]) =
    case (eval env xexpr, eval env yexpr) of
        (Number xv, Number yv) -> Number (xv - yv)
        (other, Number yv)     -> errMsg "expected-number" other
        (_, other)             -> errMsg "expected-number" other
subtract_ env other = errMsg "illegal-arguments" other

sign :: Evaluable
sign env (List [expr]) =
    let
        sgn x = if x == 0 then 0 else if x < 0 then -1 else 1
    in
        case eval env expr of
            Number xv -> Number $ sgn xv
            other -> errMsg "expected-number" other
sign env other = errMsg "illegal-arguments" other

if_ :: Evaluable
if_ env (List [test, texpr, fexpr]) =
    case eval env test of
        Boolean b -> if b then eval env texpr else eval env fexpr
        other -> errMsg "expected-boolean" other
if_ env other = errMsg "illegal-arguments" other

eval_ :: Evaluable
eval_ env (List [envlist, form]) =
    let
        newEnvVal = eval env envlist
        body      = eval env form
    in
        eval newEnvVal body
eval_ env other = errMsg "illegal-arguments" other

macro :: Evaluable
macro env (List [args@(List [(Symbol argsS), (Symbol envS)]), body]) =
    Operator "<operator>" $ makeMacro env args body
macro env other = errMsg "illegal-arguments" other

abort :: Evaluable
abort env (List [expr]) =
    Abort $ eval env expr
abort env other = errMsg "illegal-arguments" other

recover :: Evaluable
recover env (List [expr, (Symbol okName), okExpr, (Symbol abortName), abortExpr]) =
    case eval env expr of
        e@(Abort contents) -> eval (insert abortName contents env) abortExpr
        other              -> eval (insert okName other env) okExpr
recover env other = errMsg "illegal-arguments" other

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
