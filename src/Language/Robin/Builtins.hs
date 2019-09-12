module Language.Robin.Builtins where

import qualified Language.Robin.Env as Env
import Language.Robin.Expr
import Language.Robin.Eval

-- 
-- Robin Builtins
-- ==============
-- 
-- Note, these are functions which are built-in to the Robin reference
-- intepreter, for performance, but they are *not* intrinsic to the
-- Robin language.  (See Intrinsics.lhs for those.)
--

--
-- Helper functions
--

union (List []) env = env
union (List (binding:rest)) env =
    append (List [binding]) (union (List rest) env)

evalAll i env [] acc cc =
    cc $ List $ reverse acc
evalAll i env (head:tail) acc cc =
    eval i env head (\value ->
        evalAll i env tail (value:acc) cc)

--       formals actuals origActuals env i cc
evalArgs [] [] _ _ _ cc =
    cc Env.empty
evalArgs (formal@(Symbol _):formals) (actual:actuals) origActuals env i cc =
    eval i env actual (\value ->
        evalArgs formals actuals origActuals env i (\rest ->
            cc $ Env.insert formal value rest))
evalArgs _ _ origActuals _ i cc =
    raise i (errMsg "illegal-arguments" (List origActuals))

--
-- `Small`
--
-- These builtins represent the `small` package.
-- This implementation of the `small` package is non-normative.
-- See the relevant files in `stdlib` for normative definitions.
--

literal :: Evaluable
literal i env (List (expr:_)) cc =
    cc expr
literal i env other cc = raise i (errMsg "illegal-arguments" other)

robinList :: Evaluable
robinList i env (List exprs) cc =
    evalAll i env exprs [] cc

robinEnv :: Evaluable
robinEnv i env (List _) cc =
  cc env

choose :: Evaluable
choose i env (List [(List [(Symbol "else"), branch])]) cc =
    eval i env branch cc
choose i env (List ((List [test, branch]):rest)) cc =
    eval i env test (\x ->
        case x of
            Boolean True ->
                eval i env branch cc
            Boolean False ->
                choose i env (List rest) cc)
choose i env other cc = raise i (errMsg "illegal-arguments" other)

bind :: Evaluable
bind i env (List [name@(Symbol _), expr, body]) cc =
    eval i env expr (\value ->
        eval i (Env.insert name value env) body cc)
bind i env other cc = raise i (errMsg "illegal-arguments" other)

robinLet :: Evaluable
robinLet i env (List ((List bindings):body:_)) cc =
    bindAll bindings env i (\env' ->
        eval i env' body cc)
  where
    bindAll [] env ienv cc =
        cc env
    bindAll (List (name@(Symbol _):sexpr:_):rest) env ienv cc =
        eval ienv env sexpr (\value ->
            bindAll rest (Env.insert name value env) ienv cc)
    bindAll (other:rest) env ienv cc =
        raise ienv (errMsg "illegal-binding" other)
robinLet i env other cc = raise i (errMsg "illegal-arguments" other)

robinBindArgs :: Evaluable
robinBindArgs i env (List [(List formals), givenArgs, givenEnv, body]) cc =
    eval i env givenArgs (\(List actuals) ->
        eval i env givenEnv (\outerEnv ->
            evalArgs formals actuals actuals outerEnv i (\argEnv ->
                eval i (union argEnv env) body cc)))
robinBindArgs i env other cc = raise i (errMsg "illegal-arguments" other)

robinFun :: Evaluable
robinFun i closedEnv (List [(List formals), body]) cc =
    cc $ Intrinsic "<lambda>" fun
  where
    fun i env (List actuals) cc =
        evalArgs formals actuals actuals env i (\argEnv ->
            eval i (union argEnv closedEnv) body cc)
    evalArgs [] [] _ _ _ cc =
        cc Env.empty
    evalArgs (formal@(Symbol _):formals) (actual:actuals) origActuals env i cc =
        eval i env actual (\value ->
            evalArgs formals actuals origActuals env i (\rest ->
                cc $ Env.insert formal value rest))
    evalArgs _ _ origActuals _ i cc =
        raise i (errMsg "illegal-arguments" (List origActuals))
robinFun i env other cc = raise i (errMsg "illegal-arguments" other)

--
-- `Arith`
--
-- These builtins represent the `arith` package.
-- This implementation of the `arith` package is non-normative.
-- See the relevant files in `stdlib` for normative definitions.
--

robinAbs :: Evaluable
robinAbs i env (List [expr]) cc =
    eval i env expr (\n -> cc $ abs n)
    where
        abs (Number n) = Number (if n > 0 then n else 0-n)
        abs other      = raise i (errMsg "expected-number" other)
robinAbs i env other cc = raise i (errMsg "illegal-arguments" other)

robinAdd :: Evaluable
robinAdd i env (List [xexpr, yexpr]) cc =
    eval i env xexpr (\x ->
        assertNumber i x (\(Number xv) ->
            eval i env yexpr (\y ->
                assertNumber i y (\(Number yv) ->
                    cc (Number (xv + yv))))))
robinAdd i env other cc = raise i (errMsg "illegal-arguments" other)

--
-- Mapping of names to our functions, providing an evaluation environment.
--

robinBuiltins :: Expr
robinBuiltins = Env.fromList $ map (\(name,bif) -> (name, Intrinsic name bif))
      [
        ("literal",   literal),
        ("list",      robinList),
        ("bind",      bind),
        ("env",       robinEnv),
        ("let",       robinLet),
        ("choose",    choose),
        ("bind-args", robinBindArgs),
        ("fun",       robinFun),

        ("abs",       robinAbs),
        ("add",       robinAdd)
      ]
