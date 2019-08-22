module Language.Robin.TopLevel (collect) where

import Language.Robin.Expr
import qualified Language.Robin.Env as Env
import Language.Robin.Eval
import Language.Robin.Reactor


collect [] env reactors results = (env, reactors, results)

collect ((List [Symbol "display", expr]):rest) env reactors results =
    collect rest env reactors (eval (IEnv stop) env expr id:results)

collect ((List [Symbol "define", name@(Symbol _), expr]):rest) env reactors results =
    case Env.find name env of
        Just _ ->
            error ("symbol already defined: " ++ show name)
        Nothing ->
            let
                result = eval (IEnv stop) env expr id
            in
                collect rest (Env.insert name result env) reactors results

collect ((List [Symbol "reactor", facExpr, stateExpr, bodyExpr]):rest) env reactors results =
    let
        state = eval (IEnv stop) env stateExpr id
        body = eval (IEnv stop) env bodyExpr id
    in
        collect rest env ((Reactor env state body):reactors) results

collect (topExpr:rest) env reactors results =
    error ("illegal top-level form: " ++ show topExpr)
