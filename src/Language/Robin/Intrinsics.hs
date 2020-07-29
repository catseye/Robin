module Language.Robin.Intrinsics where

import Language.Robin.Expr
import Language.Robin.Env (Env, fromList, insert)
import Language.Robin.Eval


head :: Evaluable
head env (List [expr]) cc =
    eval env expr (\x ->
        assertList env x (\val ->
            case val of
                List (a:_) -> cc a
                other -> errMsg "expected-list" other))
head env other cc = errMsg "illegal-arguments" other

tail :: Evaluable
tail env (List [expr]) cc =
    eval env expr (\x ->
        assertList env x (\val ->
            case val of
                List (_:b) -> cc (List b)
                other -> errMsg "expected-list" other))
tail env other cc = errMsg "illegal-arguments" other

prepend :: Evaluable
prepend env (List [e1, e2]) cc =
    eval env e1 (\x1 -> eval env e2 (\val ->
            case val of
                List x2 -> cc $ List (x1:x2)
                other -> errMsg "expected-list" other))
prepend env other cc = errMsg "illegal-arguments" other

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

subtract :: Evaluable
subtract env (List [xexpr, yexpr]) cc =
    eval env xexpr (\x ->
        assertNumber env x (\(Number xv) ->
            eval env yexpr (\y ->
                assertNumber env y (\(Number yv) ->
                    cc (Number (xv - yv))))))
subtract env other cc = errMsg "illegal-arguments" other

sign :: Evaluable
sign env (List [expr]) cc =
    let
        sgn x = if x == 0 then 0 else if x < 0 then -1 else 1
    in
        eval env expr (\x ->
            assertNumber env x (\(Number xv) ->
                cc $ Number $ sgn xv))
sign env other cc = errMsg "illegal-arguments" other

if_ :: Evaluable
if_ env (List [test, texpr, fexpr]) cc =
    eval env test (\x ->
        assertBoolean env x (\(Boolean b) ->
            if b then eval env texpr cc else eval env fexpr cc))
if_ env other cc = errMsg "illegal-arguments" other

eval_ :: Evaluable
eval_ env (List [envlist, form]) cc =
    eval env envlist (\newEnvVal ->
        eval env form (\body ->
            eval newEnvVal body cc))
eval_ env other cc = errMsg "illegal-arguments" other

macro :: Evaluable
macro env (List [args@(List [(Symbol selfS), (Symbol argsS), (Symbol envS)]), body]) cc =
    cc $ Macro env args body
macro env other cc = errMsg "illegal-arguments" other

abort :: Evaluable
abort env (List [expr]) cc =
    eval env expr (\v -> cc $ Abort v)
abort env other cc = errMsg "illegal-arguments" other

recover :: Evaluable
recover env (List [expr, (Symbol okName), okExpr, (Symbol abortName), abortExpr]) cc =
    eval env expr (\result ->
        case result of
            e@(Abort contents) ->
                eval (insert abortName contents env) abortExpr cc
            other ->
                eval (insert okName other env) okExpr cc)
recover env other cc = errMsg "illegal-arguments" other

robinIntrinsics :: Env
robinIntrinsics = fromList $ map (\(name,bif) -> (name, Builtin name bif))
      [
        ("head",     Language.Robin.Intrinsics.head),
        ("tail",     Language.Robin.Intrinsics.tail),
        ("prepend",  prepend),
        ("list?",    listP),
        ("symbol?",  symbolP),
        ("macro?",   macroP),
        ("number?",  numberP),
        ("equal?",   equalP),
        ("subtract", Language.Robin.Intrinsics.subtract),
        ("sign",     sign),
        ("macro",    macro),
        ("eval",     eval_),
        ("if",       if_),
        ("abort",    abort),
        ("recover",  recover)
      ]
