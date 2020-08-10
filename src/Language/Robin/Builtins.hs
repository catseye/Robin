module Language.Robin.Builtins where

import Data.Int

import Language.Robin.Expr
import Language.Robin.Env (Env, fromList, mergeEnvs, empty, insert)
import Language.Robin.Eval

-- 
-- Robin Builtins
-- ==============
-- 
-- Note, these are operators which are built-in to the Robin reference
-- intepreter, for performance, but they are *not* intrinsic to the
-- Robin language.  (See Intrinsics.hs for those.)
--

--
-- Helper functions
--

evalAll env [] acc cc =
    cc $ List $ reverse acc
evalAll env (head:tail) acc cc =
    evalB cc env head (\value ->
        evalAll env tail (value:acc) cc)

--          errcont          formals   actuals   origActuals env    continuation
evalArgs :: (Env -> Expr) -> [Expr] -> [Expr] -> [Expr] ->   Env -> (Env -> Expr) -> Expr
evalArgs ecc [] [] _ _ cc =
    cc empty
evalArgs ecc ((Symbol formal):formals) (actual:actuals) origActuals env cc =
    evalB ecc env actual (\value ->
        evalArgs ecc formals actuals origActuals env (\nenv ->
            cc $ insert formal value nenv))
evalArgs ecc _ _ origActuals env _ =
    errMsg ecc "illegal-arguments" $ List origActuals


evalTwoNumbers :: (Int32 -> Int32 -> (Expr -> Expr) -> Expr) -> Evaluable
evalTwoNumbers fn env (List [xexpr, yexpr]) cc =
    evalB cc env xexpr (\x ->
        assertNumber cc env x (\(Number xv) ->
            evalB cc env yexpr (\y ->
                assertNumber cc env y (\(Number yv) ->
                    (fn xv yv cc)))))
evalTwoNumbers fn env other cc = errMsg cc "illegal-arguments" other

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
literal env other cc = errMsg cc "illegal-arguments" other

list :: Evaluable
list env (List exprs) cc =
    evalAll env exprs [] cc

env_ :: Evaluable
env_ env (List _) cc =
    cc $ env

choose :: Evaluable
choose env (List [(List [(Symbol "else"), branch])]) cc =
    eval env branch cc
choose env (List ((List [test, branch]):rest)) cc =
    evalB cc env test (\x ->
        case x of
            Boolean True ->
                eval env branch cc
            Boolean False ->
                choose env (List rest) cc)
choose env other cc = errMsg cc "illegal-arguments" other

bind :: Evaluable
bind env (List [(Symbol name), expr, body]) cc =
    evalB cc env expr (\value ->
        eval (insert name value env) body cc)
bind env other cc = errMsg cc "illegal-arguments" other

let_ :: Evaluable
let_ env (List ((List bindings):body:_)) cc =
    bindAll cc bindings env (\env' ->
        eval env' body cc)
  where
    bindAll ecc [] env cc =
        cc env
    bindAll ecc (List ((Symbol name):sexpr:_):rest) env cc =
        evalB ecc env sexpr (\value ->
            bindAll ecc rest (insert name value env) cc)
    bindAll ecc (other:rest) env cc =
        errMsg ecc "illegal-binding" other
let_ env other cc = errMsg cc "illegal-arguments" other

bindArgs :: Evaluable
bindArgs env (List [(List formals), givenArgs, givenEnvExpr, body]) cc =
    evalB cc env givenArgs (\(List actuals) ->
        evalB cc env givenEnvExpr (\outerEnvExpr ->
            evalArgs cc formals actuals actuals outerEnvExpr (\argEnv ->
                eval (mergeEnvs argEnv env) body cc)))
bindArgs env other cc = errMsg cc "illegal-arguments" other

fun :: Evaluable
fun closedEnv (List [(List formals), body]) cc =
    cc $ Operator "<lambda>" fun
  where
    fun env (List actuals) cc =
        evalArgs cc formals actuals actuals env (\argEnv ->
            eval (mergeEnvs argEnv closedEnv) body cc)
fun env other cc = errMsg cc "illegal-arguments" other

--
-- `Arith`
--
-- These builtins represent the `arith` package.
-- This implementation of the `arith` package is non-normative.
-- See the relevant files in `stdlib` for normative definitions.
--

gtP :: Evaluable
gtP = evalTwoNumbers (\x y cc -> cc $ Boolean (x > y))

gteP :: Evaluable
gteP = evalTwoNumbers (\x y cc -> cc $ Boolean (x >= y))

ltP :: Evaluable
ltP = evalTwoNumbers (\x y cc -> cc $ Boolean (x < y))

lteP :: Evaluable
lteP = evalTwoNumbers (\x y cc -> cc $ Boolean (x <= y))

abs_ :: Evaluable
abs_ env (List [expr]) cc =
    eval env expr (\x -> assertNumber (cc) env x (\(Number xv) -> cc (Number $ abs xv)))
abs_ env other cc = errMsg cc "illegal-arguments" other

add :: Evaluable
add = evalTwoNumbers (\x y cc -> cc $ Number (x + y))

multiply :: Evaluable
multiply = evalTwoNumbers (\x y cc -> cc $ Number (x * y))

divide :: Evaluable
divide = evalTwoNumbers (\x y cc -> case y of
                                 0 -> errMsg cc "division-by-zero" $ Number x
                                 _ -> cc $ Number (x `div` y))

remainder :: Evaluable
remainder = evalTwoNumbers (\x y cc -> case y of
                                 0 -> errMsg cc "division-by-zero" $ Number x
                                 _ -> cc $ Number (abs (x `mod` y)))

--
-- Mapping of names to our functions, providing an evaluation environment.
--

robinBuiltins :: Env
robinBuiltins = fromList $ map (\(name,bif) -> (name, Operator name bif))
      [
        ("literal",   literal),
        ("list",      list),
        ("env",       env_),
        ("choose",    choose),
        ("bind",      bind),
        ("let",       let_),
        ("bind-args", bindArgs),
        ("fun",       fun),

        ("gt?",       gtP),
        ("gte?",      gteP),
        ("lt?",       ltP),
        ("lte?",      lteP),

        ("abs",       abs_),
        ("add",       add),
        ("multiply",  multiply),
        ("divide",    divide),
        ("remainder", remainder)
      ]
