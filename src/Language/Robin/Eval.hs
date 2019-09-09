module Language.Robin.Eval where

import qualified Language.Robin.Env as Env
import Language.Robin.Expr

--
-- This evaluator is written in continuation-passing style.
--
-- Every evaluation function takes a continuation, and is implemented
-- as a function with this signature:
--
--     Expr -> Expr -> Expr -> (Expr -> Expr) -> Expr
--
-- (This is actually the `Evaluable` type from `Robin.Expr`.)
--
-- The first argument is the internal context, which contains things like the
-- exception handler, etc.
--
-- The second argument is the Robin environment, which is directly visible
-- (and modifiable, during `eval`) by the Robin program.
--
-- When evaluating a symbol, look it up in the environment to obtain a
-- value.  Then continue the current continuation with that value.
--

eval :: Evaluable

eval i (List []) s@(Symbol _) cc =
    raise i (errMsg "unbound-identifier" s)
eval i (List (b@(List [id@(Symbol _), value]):env)) s@(Symbol _) cc
    | id == s   = cc value
    | otherwise = eval i (List env) s cc
eval i (List ((List (other:_)):env)) s@(Symbol _) cc =
    raise i (errMsg "expected-symbol" other)
eval i (List (head:tail)) s@(Symbol _) cc =
    raise i (errMsg "expected-env-entry" head)
eval i env s@(Symbol _) cc =
    raise i (errMsg "expected-env-alist" env)

--
-- Evaluating a list means we must make several evaluations.  We
-- evaluate the head to obtain something to apply (which must be a
-- macro or intrinsic.)  We then apply the body of the macro,
-- passing it the tail of the list.
--

eval i env (List (applierExpr:actuals)) cc =
    eval i env applierExpr (\applier ->
        case applier of
            m@(Macro _ _ body) ->
                eval i (makeMacroEnv env (List actuals) m) body cc
            b@(Intrinsic _ fun) ->
                fun i env (List actuals) cc
            other ->
                raise i (errMsg "inapplicable-object" other))

--
-- Everything else just evaluates to itself.  Continue the current
-- continuation with that value.
--

eval i env e cc =
    cc e

--
-- Helper functions
--

errMsg msg term =
    List [(Symbol msg), term]

makeMacroEnv env actuals m@(Macro closedEnv argList _)  =
    let
        (List [argSelf@(Symbol _), argFormal@(Symbol _),
               envFormal@(Symbol _)]) = argList
        newEnv = Env.insert argSelf m closedEnv
        newEnv' = Env.insert argFormal actuals newEnv
        newEnv'' = Env.insert envFormal env newEnv'
    in
        newEnv''

--
-- Exception Handler
--

raise :: IEnv Expr -> Expr -> Expr
raise i expr =
    (getExceptionHandler i) expr

--
-- Assertions
--

assert i pred msg expr cc =
    case pred expr of
        True -> cc expr
        False -> raise i (errMsg msg expr)

assertSymbol i = assert i (isSymbol) "expected-symbol"
assertBoolean i = assert i (isBoolean) "expected-boolean"
assertList i = assert i (isList) "expected-list"
assertNumber i = assert i (isNumber) "expected-number"
assertMacro i = assert i (isMacro) "expected-macro"
