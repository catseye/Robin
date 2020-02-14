module Language.Robin.Builtins where

import Data.Int

import Language.Robin.Expr
import Language.Robin.Eval

-- 
-- Robin Builtins
-- ==============
-- 
-- Note, these are functions which are built-in to the Robin reference
-- intepreter, for performance, but they are *not* intrinsic to the
-- Robin language.  (See Intrinsics.hs for those.)
--

--
-- Helper functions
--

evalAll env [] acc cc =
    cc $ List $ reverse acc
evalAll env (head:tail) acc cc =
    eval env head (\value ->
        evalAll env tail (value:acc) cc)

--          formals   actuals   origActuals env    kont
evalArgs :: [Expr] -> [Expr] -> [Expr] ->   Env -> (Env -> Expr) -> Expr
evalArgs [] [] _ _ cc =
    cc empty
evalArgs ((Symbol formal):formals) (actual:actuals) origActuals env cc =
    eval env actual (\value ->
        evalArgs formals actuals origActuals env (\nenv ->
            cc $ insert formal value nenv))
evalArgs _ _ origActuals env cc =
    errMsg "illegal-arguments" $ List origActuals


evalTwoNumbers :: (Int32 -> Int32 -> (Expr -> Expr) -> Expr) -> Evaluable
evalTwoNumbers fn env (List [xexpr, yexpr]) cc =
    eval env xexpr (\x ->
        assertNumber env x (\(Number xv) ->
            eval env yexpr (\y ->
                assertNumber env y (\(Number yv) ->
                    (fn xv yv cc)))))
evalTwoNumbers fn env other cc = errMsg "illegal-arguments" other

--
-- `Small`
--
-- These builtins represent the `small` package.
-- This implementation of the `small` package is non-normative.
-- See the relevant files in `stdlib` for normative definitions.
--

literal :: Evaluable
literal env (List (expr:_)) cc =
    cc expr
literal env other cc = errMsg "illegal-arguments" other

robinList :: Evaluable
robinList env (List exprs) cc =
    evalAll env exprs [] cc

robinEnv :: Evaluable
robinEnv env (List _) cc =
    cc $ env

choose :: Evaluable
choose env (List [(List [(Symbol "else"), branch])]) cc =
    eval env branch cc
choose env (List ((List [test, branch]):rest)) cc =
    eval env test (\x ->
        case x of
            Boolean True ->
                eval env branch cc
            Boolean False ->
                choose env (List rest) cc)
choose env other cc = errMsg "illegal-arguments" other

bind :: Evaluable
bind env (List [(Symbol name), expr, body]) cc =
    eval env expr (\value ->
        eval (insert name value env) body cc)
bind env other cc = errMsg "illegal-arguments" other

robinLet :: Evaluable
robinLet env (List ((List bindings):body:_)) cc =
    bindAll bindings env (\env' ->
        eval env' body cc)
  where
    bindAll [] env cc =
        cc env
    bindAll (List ((Symbol name):sexpr:_):rest) env cc =
        eval env sexpr (\value ->
            bindAll rest (insert name value env) cc)
    bindAll (other:rest) env cc =
        errMsg "illegal-binding" other
robinLet env other cc = errMsg "illegal-arguments" other

robinBindArgs :: Evaluable
robinBindArgs env (List [(List formals), givenArgs, givenEnvExpr, body]) cc =
    eval env givenArgs (\(List actuals) ->
        eval env givenEnvExpr (\outerEnvExpr ->
            evalArgs formals actuals actuals outerEnvExpr (\argEnv ->
                eval (mergeEnvs argEnv env) body cc)))
robinBindArgs env other cc = errMsg "illegal-arguments" other

robinFun :: Evaluable
robinFun closedEnv (List [(List formals), body]) cc =
    cc $ Intrinsic "<lambda>" fun
  where
    fun env (List actuals) cc =
        evalArgs formals actuals actuals env (\argEnv ->
            eval (mergeEnvs argEnv closedEnv) body cc)
robinFun env other cc = errMsg "illegal-arguments" other

--
-- `Arith`
--
-- These builtins represent the `arith` package.
-- This implementation of the `arith` package is non-normative.
-- See the relevant files in `stdlib` for normative definitions.
--

robinGt :: Evaluable
robinGt = evalTwoNumbers (\x y cc -> cc $ Boolean (x > y))

robinGte :: Evaluable
robinGte = evalTwoNumbers (\x y cc -> cc $ Boolean (x >= y))

robinLt :: Evaluable
robinLt = evalTwoNumbers (\x y cc -> cc $ Boolean (x < y))

robinLte :: Evaluable
robinLte = evalTwoNumbers (\x y cc -> cc $ Boolean (x <= y))

robinAbs :: Evaluable
robinAbs env (List [expr]) cc =
    eval env expr (\x -> assertNumber env x (\(Number xv) -> cc (Number $ abs xv)))
robinAbs env other cc = errMsg "illegal-arguments" other

robinAdd :: Evaluable
robinAdd = evalTwoNumbers (\x y cc -> cc $ Number (x + y))

robinMultiply :: Evaluable
robinMultiply = evalTwoNumbers (\x y cc -> cc $ Number (x * y))

robinDivide :: Evaluable
robinDivide env (List [xexpr, yexpr]) cc =
    eval env xexpr (\x ->
        assertNumber env x (\(Number xv) ->
            eval env yexpr (\y ->
                assertNumber env y (\(Number yv) ->
                    case yv of
                        0 -> errMsg "division-by-zero" $ Number xv
                        _ -> cc $ Number (xv `div` yv)))))
robinDivide env other cc = errMsg "illegal-arguments" other

robinRemainder :: Evaluable
robinRemainder env (List [xexpr, yexpr]) cc =
    eval env xexpr (\x ->
        assertNumber env x (\(Number xv) ->
            eval env yexpr (\y ->
                assertNumber env y (\(Number yv) ->
                    case yv of
                        0 -> errMsg "division-by-zero" $ Number xv
                        _ -> cc $ Number (abs (xv `mod` yv))))))
robinRemainder env other cc = errMsg "illegal-arguments" other

--
-- Mapping of names to our functions, providing an evaluation environment.
--

robinBuiltins :: Env
robinBuiltins = fromList $ map (\(name,bif) -> (name, Intrinsic name bif))
      [
        ("literal",   literal),
        ("list",      robinList),
        ("env",       robinEnv),
        ("choose",    choose),
        ("bind",      bind),
        ("let",       robinLet),
        ("bind-args", robinBindArgs),
        ("fun",       robinFun),

        ("gt?",       robinGt),
        ("gte?",      robinGte),
        ("lt?",       robinLt),
        ("lte?",      robinLte),

        ("abs",       robinAbs),
        ("add",       robinAdd),
        ("multiply",  robinMultiply),
        ("divide",    robinDivide),
        ("remainder", robinRemainder)
      ]
