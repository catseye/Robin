module Language.Robin.Eval where

import Language.Robin.Expr

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
-- macro or intrinsic.)  We then apply the body of the macro,
-- passing it the tail of the list.
--

eval env (List (applierExpr:actuals)) cc =
    eval env applierExpr (\applier ->
        case applier of
            m@(Macro _ _ body) ->
                eval (makeMacroEnv env (List actuals) m) body cc
            b@(Intrinsic _ fun) ->
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
    Error (List [(Symbol msg), term])

makeMacroEnv :: Env -> Expr -> Expr -> Env
makeMacroEnv env actuals m@(Macro closedEnv argList _)  =
    let
        (List [(Symbol argSelf), (Symbol argFormal),
               (Symbol envFormal)]) = argList
        newEnv = insert argSelf m closedEnv
        newEnv' = insert argFormal actuals newEnv
        newEnv'' = insert envFormal env newEnv'
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
assertMacro env = assert env (isMacro) "expected-macro"
