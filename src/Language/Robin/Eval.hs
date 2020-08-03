module Language.Robin.Eval where

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
            errMsg "unbound-identifier" sym

--
-- Evaluating a list means we must make several evaluations.  We
-- evaluate the head to obtain something to apply (which must be a
-- "builtin".)  We then apply the "builtin", passing it the tail of the list.
--

eval env (List (applierExpr:actuals)) cc =
    eval env applierExpr (\applier ->
        case applier of
            Builtin _ fun ->
                fun env (List actuals) cc
            other ->
                errMsg "inapplicable-object" other)

--
-- Everything else just evaluates to itself.  Continue the current
-- continuation with that value.
--

eval env e cc =
    cc e

--
-- Helper functions
--

errMsg msg term =
    Abort (List [(Symbol msg), term])

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

assert env pred msg expr cc =
    case pred expr of
        True -> cc expr
        False -> errMsg msg expr

assertSymbol env = assert env (isSymbol) "expected-symbol"
assertBoolean env = assert env (isBoolean) "expected-boolean"
assertList env = assert env (isList) "expected-list"
assertNumber env = assert env (isNumber) "expected-number"
assertOperator env = assert env (isOperator) "expected-operator"
