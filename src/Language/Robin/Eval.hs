module Language.Robin.Eval where

import Data.Int

import Language.Robin.Expr
import Language.Robin.Env (Env, find, insert)

--
-- This evaluator is written in continuation-passing style.
--
-- Every evaluation function has this signature:
--
--     Env -> Expr -> (Expr -> Expr) -> Expr
--
-- (This is actually the `Evaluable` type from `Robin.Expr`.)
--
-- The first argument is the Robin environment, which is directly visible
-- (and modifiable, during `eval`) by the Robin program.
--
-- The second argument is the expression to be evaluated.
--
-- The third argument is the continuation.  Once the expression has been
-- evaluated, the continuation will be applied with the result of the
-- evaluation.
--

eval :: Evaluable

--
-- When evaluating a symbol, look it up in the environment to obtain a
-- value.  Then continue the current continuation with that value.
--

eval env sym@(Symbol s) cc =
    case find s env of
        Just value ->
            cc value
        Nothing ->
            errMsg cc "unbound-identifier" sym

--
-- Evaluating a list means we must make several evaluations.  We
-- evaluate the head to obtain something to apply, which must be an
-- operator.  We then apply the operator, passing it the tail of the list.
--

eval env (List (applierExpr:actuals)) cc =
    eval env applierExpr (\applier ->
        case applier of
            Operator _ fun ->
                fun env (List actuals) cc
            other ->
                errMsg cc "inapplicable-object" other)

--
-- Everything else just evaluates to itself.  Continue the current
-- continuation with that value.
--

eval env e cc =
    cc e

--
-- Evaluate, handling abort values
--

evalB ecc env e cc =
    eval env e (\value ->
        case value of
            Abort _ -> ecc value
            _       -> cc value)

--
-- Helper functions
--

errMsg cc msg term = cc $ abortVal msg term

makeMacro :: Expr -> Expr -> Expr -> Evaluable
makeMacro defineTimeEnv formals body =
    \callTimeEnv actuals cc ->
        let
            env = makeMacroEnv callTimeEnv actuals defineTimeEnv formals
        in
            eval env body cc

makeMacroEnv callTimeEnv actuals defineTimeEnv argList =
    let
        (List [(Symbol argFormal), (Symbol envFormal)]) = argList
        newEnv' = insert argFormal actuals defineTimeEnv
        newEnv'' = insert envFormal callTimeEnv newEnv'
    in
        newEnv''


--
-- Assertions
--

assert ecc env pred msg expr cc =
    case pred expr of
        True -> cc expr
        False -> errMsg ecc msg expr

assertList ecc env expr = assert ecc env (isList) "expected-list" expr

evalExpect pred msg ecc env expr cc =
   eval env expr (\value ->
       case pred value of
           True -> cc value
           False -> errMsg ecc msg value)

evalToBoolean = evalExpect (isBoolean) "expected-boolean"
evalToList = evalExpect (isList) "expected-list"
evalToNumber = evalExpect (isNumber) "expected-number"

evalTwoNumbers :: (Int32 -> Int32 -> (Expr -> Expr) -> Expr) -> Evaluable
evalTwoNumbers fn env (List [xexpr, yexpr]) cc =
    evalToNumber cc env xexpr (\(Number xv) ->
        evalToNumber cc env yexpr (\(Number yv) ->
            (fn xv yv cc)))
evalTwoNumbers fn env other cc = errMsg cc "illegal-arguments" other
