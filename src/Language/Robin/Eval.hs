module Language.Robin.Eval where

import Language.Robin.Expr
import Language.Robin.Env (Env, find, insert)

--
-- This evaluator is written in continuation-passing style.
--
-- The evaluation function has this signature:
--
--     Env -> Expr -> Expr
--
-- (This is actually the `Evaluable` type from `Robin.Expr`.)
--
-- The first argument is the Robin environment, which is directly visible
-- (and modifiable, during `eval`) by the Robin program.
--
-- The second argument is the expression to be evaluated.
--

eval :: Evaluable

--
-- When evaluating a symbol, look it up in the environment to obtain a
-- value.  Then continue the current continuation with that value.
--

eval env sym@(Symbol s) =
    case find s env of
        Just value ->
            value
        Nothing ->
            errMsg "unbound-identifier" (List [sym, env])

--
-- When evaluating a list, we evaluate the head to obtain something to apply,
-- which must be an operator.  We then apply the operator, passing it the
-- tail of the list.
--

eval env (List (applierExpr:actuals)) =
    case eval env applierExpr of
        Operator _ op ->
            op env (List actuals)
        other ->
            errMsg "inapplicable-object" other

--
-- Everything else just evaluates to itself.
--

eval env e = e

--
-- Helper functions
--

makeMacro :: Expr -> Expr -> Expr -> Evaluable
makeMacro defineTimeEnv formals body =
    \callTimeEnv actuals ->
        let
            env = makeMacroEnv callTimeEnv actuals defineTimeEnv formals
        in
            eval env body

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

assert env pred msg expr cc =
    case pred expr of
        True -> cc expr
        False -> errMsg msg expr

assertSymbol env = assert env (isSymbol) "expected-symbol"
assertBoolean env = assert env (isBoolean) "expected-boolean"
assertList env = assert env (isList) "expected-list"
assertNumber env = assert env (isNumber) "expected-number"
assertOperator env = assert env (isOperator) "expected-operator"
