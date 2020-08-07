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

evalAll env [] acc =
    List $ reverse acc
evalAll env (head:tail) acc =
    let
        value = (eval env head)
    in
        evalAll env tail (value:acc)

--          formals   actuals   origActuals env
evalArgs :: [Expr] -> [Expr] -> [Expr] ->   Env -> Expr
evalArgs [] [] _ _ =
    empty
evalArgs ((Symbol formal):formals) (actual:actuals) origActuals env =
    let
        value = eval env actual
        nenv = evalArgs formals actuals origActuals env
    in
        insert formal value nenv
evalArgs _ _ origActuals env =
    errMsg "illegal-arguments" $ List origActuals


evalTwoNumbers :: (Int32 -> Int32 -> Expr) -> Evaluable
evalTwoNumbers fn env (List [xexpr, yexpr]) =
    case (eval env xexpr, eval env yexpr) of
        (Number xv, Number yv) -> fn xv yv
        (other, Number yv)     -> errMsg "expected-number" other
        (_, other)             -> errMsg "expected-number" other
evalTwoNumbers fn env other = errMsg "illegal-arguments" other

--
-- `Small`
--
-- These builtins represent the `small` package.
-- This implementation of the `small` package is non-normative.
-- See the relevant files in `stdlib` for normative definitions.
--

literal :: Evaluable
literal env (List (expr:_)) = expr
literal env other = errMsg "illegal-arguments" other

list :: Evaluable
list env (List exprs) =
    evalAll env exprs []

env_ :: Evaluable
env_ env (List _) = env

choose :: Evaluable
choose env (List [(List [(Symbol "else"), branch])]) =
    eval env branch
choose env (List ((List [test, branch]):rest)) =
    case eval env test of
        Boolean True ->
            eval env branch
        Boolean False ->
            choose env (List rest)
choose env other = errMsg "illegal-arguments" other

bind :: Evaluable
bind env (List [(Symbol name), expr, body]) =
    let
        value = eval env expr
        env' = insert name value env
    in
        eval env' body
bind env other = errMsg "illegal-arguments" other

let_ :: Evaluable
let_ env (List ((List bindings):body:_)) =
    let
        env' = bindAll bindings env
    in
        eval env' body
    where
        bindAll [] env = env
        bindAll (List ((Symbol name):expr:_):rest) env =
            let
                value = eval env expr
                env' = insert name value env
            in
                bindAll rest env'
        bindAll (other:rest) env =
            errMsg "illegal-binding" other
let_ env other = errMsg "illegal-arguments" other

bindArgs :: Evaluable
bindArgs env (List [(List formals), givenArgs, givenEnvExpr, body]) =
    let
        List actuals = eval env givenArgs
        outerEnvExpr = eval env givenEnvExpr
        argEnv = evalArgs formals actuals actuals outerEnvExpr
    in
        eval (mergeEnvs argEnv env) body
bindArgs env other = errMsg "illegal-arguments" other

fun :: Evaluable
fun closedEnv (List [(List formals), body]) =
    let
        fun env (List actuals) =
            let
                argEnv = evalArgs formals actuals actuals env
                env' = mergeEnvs argEnv closedEnv
            in
                eval env' body
    in
        Operator "<lambda>" fun
fun env other = errMsg "illegal-arguments" other

--
-- `Arith`
--
-- These builtins represent the `arith` package.
-- This implementation of the `arith` package is non-normative.
-- See the relevant files in `stdlib` for normative definitions.
--

gtP :: Evaluable
gtP = evalTwoNumbers (\x y -> Boolean (x > y))

gteP :: Evaluable
gteP = evalTwoNumbers (\x y -> Boolean (x >= y))

ltP :: Evaluable
ltP = evalTwoNumbers (\x y -> Boolean (x < y))

lteP :: Evaluable
lteP = evalTwoNumbers (\x y -> Boolean (x <= y))

abs_ :: Evaluable
abs_ env (List [expr]) =
    case eval env expr of
        Number xv -> Number $ abs xv
        other     -> errMsg "expected-number" other
abs_ env other = errMsg "illegal-arguments" other

add :: Evaluable
add = evalTwoNumbers (\x y -> Number (x + y))

multiply :: Evaluable
multiply = evalTwoNumbers (\x y -> Number (x * y))

divide :: Evaluable
divide = evalTwoNumbers (\x y -> case y of
                                 0 -> errMsg "division-by-zero" $ Number x
                                 _ -> Number (x `div` y))

remainder :: Evaluable
remainder = evalTwoNumbers (\x y -> case y of
                                 0 -> errMsg "division-by-zero" $ Number x
                                 _ -> Number (abs (x `mod` y)))

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
