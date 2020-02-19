module Language.Robin.Builtins where

import Prelude (
    ($), (>), (>=), (<), (<=), (+), (*), div, mod, map, reverse, Bool(True, False)
  )
import qualified Prelude as P

import Data.Int

import Language.Robin.Expr
import Language.Robin.Env (Env, fromList, mergeEnvs, empty, insert)
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

--          formals   actuals   origActuals env    continuation
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

let_ :: Evaluable
let_ env (List ((List bindings):body:_)) cc =
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
let_ env other cc = errMsg "illegal-arguments" other

bindArgs :: Evaluable
bindArgs env (List [(List formals), givenArgs, givenEnvExpr, body]) cc =
    eval env givenArgs (\(List actuals) ->
        eval env givenEnvExpr (\outerEnvExpr ->
            evalArgs formals actuals actuals outerEnvExpr (\argEnv ->
                eval (mergeEnvs argEnv env) body cc)))
bindArgs env other cc = errMsg "illegal-arguments" other

fun :: Evaluable
fun closedEnv (List [(List formals), body]) cc =
    cc $ Intrinsic "<lambda>" fun
  where
    fun env (List actuals) cc =
        evalArgs formals actuals actuals env (\argEnv ->
            eval (mergeEnvs argEnv closedEnv) body cc)
fun env other cc = errMsg "illegal-arguments" other

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

abs :: Evaluable
abs env (List [expr]) cc =
    eval env expr (\x -> assertNumber env x (\(Number xv) -> cc (Number $ P.abs xv)))
abs env other cc = errMsg "illegal-arguments" other

add :: Evaluable
add = evalTwoNumbers (\x y cc -> cc $ Number (x + y))

multiply :: Evaluable
multiply = evalTwoNumbers (\x y cc -> cc $ Number (x * y))

divide :: Evaluable
divide = evalTwoNumbers (\x y cc -> case y of
                                 0 -> errMsg "division-by-zero" $ Number x
                                 _ -> cc $ Number (x `div` y))

remainder :: Evaluable
remainder = evalTwoNumbers (\x y cc -> case y of
                                 0 -> errMsg "division-by-zero" $ Number x
                                 _ -> cc $ Number (P.abs (x `mod` y)))

--
-- Mapping of names to our functions, providing an evaluation environment.
--

robinBuiltins :: Env
robinBuiltins = fromList $ map (\(name,bif) -> (name, Intrinsic name bif))
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

        ("abs",       abs),
        ("add",       add),
        ("multiply",  multiply),
        ("divide",    divide),
        ("remainder", remainder)
      ]
