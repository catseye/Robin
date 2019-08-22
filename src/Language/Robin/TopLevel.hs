module Language.Robin.TopLevel (collect) where

import Language.Robin.Expr
import qualified Language.Robin.Env as Env
import Language.Robin.Eval
import Language.Robin.Reactor


collect [] env reactors results = (env, reactors, results)

collect ((List [Symbol "display", expr]):rest) env reactors results =
    collect rest env reactors (eval (IEnv stop) env expr id:results)

collect ((List [Symbol "assert", expr]):rest) env reactors results =
    case eval (IEnv stop) env expr id of
        Boolean False ->
            error ("assertion failed: " ++ show expr)
        _ ->
            collect rest env reactors results

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
        newReactor = Reactor{ rid=(fromIntegral $ length reactors), env=env, state=state, body=body }
    in
        collect rest env (newReactor:reactors) results

collect (topExpr:rest) env reactors results =
    error ("illegal top-level form: " ++ show topExpr)
