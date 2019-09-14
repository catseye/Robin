module Language.Robin.Builtins where

import qualified Language.Robin.Env as Env
import Language.Robin.Env (Env)
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

--          formals   actuals   origActuals env         i            wierd-cc
evalArgs :: [Expr] -> [Expr] -> [Expr] ->   Env Expr -> IEnv Expr -> (Env Expr -> Expr) -> Expr
evalArgs formals actuals origActuals env i cc =
    evalArgs' formals actuals origActuals env i cc
    where
        evalArgs' [] [] _ _ _ cc =
            cc Env.empty
        evalArgs' ((Symbol formal):formals) (actual:actuals) origActuals env i cc =
            eval i env actual (\value ->
                evalArgs' formals actuals origActuals env i (\nenv ->
                    cc $ Env.insert formal value nenv))
        evalArgs' _ _ origActuals _ i cc =
            raise i (errMsg "illegal-arguments" (List origActuals))

--              formals   actuals   origActuals envExpr i            wierd-cc
evalArgsExpr :: [Expr] -> [Expr] -> [Expr] ->   Expr -> IEnv Expr -> (Env Expr -> Expr) -> Expr
evalArgsExpr formals actuals origActuals envExpr i cc =
    case exprToEnv envExpr of
        Right env ->
            evalArgs formals actuals origActuals env i cc
        Left (msg, value) ->
            raise i (errMsg msg value)

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
    cc $ envToExpr env

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
bind i env (List [(Symbol name), expr, body]) cc =
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
    bindAll (List ((Symbol name):sexpr:_):rest) env ienv cc =
        eval ienv env sexpr (\value ->
            bindAll rest (Env.insert name value env) ienv cc)
    bindAll (other:rest) env ienv cc =
        raise ienv (errMsg "illegal-binding" other)
robinLet i env other cc = raise i (errMsg "illegal-arguments" other)

robinBindArgs :: Evaluable
robinBindArgs i env (List [(List formals), givenArgs, givenEnvExpr, body]) cc =
    eval i env givenArgs (\(List actuals) ->
        eval i env givenEnvExpr (\outerEnvExpr ->
            evalArgsExpr formals actuals actuals outerEnvExpr i (\argEnv ->
                eval i (Env.mergeEnvs argEnv env) body cc)))
robinBindArgs i env other cc = raise i (errMsg "illegal-arguments" other)

robinFun :: Evaluable
robinFun i closedEnv (List [(List formals), body]) cc =
    cc $ Intrinsic "<lambda>" fun
  where
    fun i env (List actuals) cc =
        evalArgs formals actuals actuals env i (\argEnv ->
            eval i (Env.mergeEnvs argEnv closedEnv) body cc)
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
    eval i env expr (\x -> assertNumber i x (\(Number xv) -> cc (Number $ abs xv)))
robinAbs i env other cc = raise i (errMsg "illegal-arguments" other)

robinAdd :: Evaluable
robinAdd i env (List [xexpr, yexpr]) cc =
    eval i env xexpr (\x ->
        assertNumber i x (\(Number xv) ->
            eval i env yexpr (\y ->
                assertNumber i y (\(Number yv) ->
                    cc (Number (xv + yv))))))
robinAdd i env other cc = raise i (errMsg "illegal-arguments" other)

robinMultiply :: Evaluable
robinMultiply i env (List [xexpr, yexpr]) cc =
    eval i env xexpr (\x ->
        assertNumber i x (\(Number xv) ->
            eval i env yexpr (\y ->
                assertNumber i y (\(Number yv) ->
                    cc (Number (xv * yv))))))
robinMultiply i env other cc = raise i (errMsg "illegal-arguments" other)

robinDivide :: Evaluable
robinDivide i env (List [xexpr, yexpr]) cc =
    eval i env xexpr (\x ->
        assertNumber i x (\(Number xv) ->
            eval i env yexpr (\y ->
                assertNumber i y (\(Number yv) ->
                    case yv of
                        0 -> raise i (errMsg "division-by-zero" (Number xv))
                        _ -> cc (Number (xv `div` yv))))))
robinDivide i env other cc = raise i (errMsg "illegal-arguments" other)

robinRemainder :: Evaluable
robinRemainder i env (List [xexpr, yexpr]) cc =
    eval i env xexpr (\x ->
        assertNumber i x (\(Number xv) ->
            eval i env yexpr (\y ->
                assertNumber i y (\(Number yv) ->
                    case yv of
                        0 -> raise i (errMsg "division-by-zero" (Number xv))
                        _ -> cc (Number (abs (xv `mod` yv)))))))
robinRemainder i env other cc = raise i (errMsg "illegal-arguments" other)

--
-- Mapping of names to our functions, providing an evaluation environment.
--

robinBuiltins :: Env Expr
robinBuiltins = Env.fromList $ map (\(name,bif) -> (name, Intrinsic name bif))
      [
        ("literal",   literal),
        ("list",      robinList),
        ("env",       robinEnv),
        ("choose",    choose),
        ("bind",      bind),
        ("let",       robinLet),
        ("bind-args", robinBindArgs),
        ("fun",       robinFun),

        ("abs",       robinAbs),
        ("add",       robinAdd),
        ("multiply",  robinMultiply),
        ("divide",    robinDivide),
        ("remainder", robinRemainder)
      ]
