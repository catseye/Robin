module Language.Robin.Eval where

import qualified Language.Robin.Env as Env
import Language.Robin.Expr

--
-- This evaluator is written in continuation-passing style.
--
-- Every evaluation function takes a continuation, and is implemented
-- as a function with this signature:
--
--     IEnv Expr -> Env Expr -> Expr -> (Expr -> Expr) -> Expr
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

eval i env sym@(Symbol s) cc =
    case Env.find s env of
        Just value ->
            cc value
        Nothing ->
            raise i (errMsg "unbound-identifier" sym)

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

makeMacroEnv :: Env.Env Expr -> Expr -> Expr -> Env.Env Expr
makeMacroEnv env actuals m@(Macro closedEnv argList _)  =
    let
        (List [(Symbol argSelf), (Symbol argFormal),
               (Symbol envFormal)]) = argList
        newEnv = Env.insert argSelf m closedEnv
        newEnv' = Env.insert argFormal actuals newEnv
        newEnv'' = Env.insert envFormal (Environment env) newEnv'
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
