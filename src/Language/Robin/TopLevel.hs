module Language.Robin.TopLevel (collect) where

import qualified Language.Robin.Env as Env
import Language.Robin.Env (Env)
import Language.Robin.Expr
import Language.Robin.Eval
import Language.Robin.Reactor


collect :: [Expr] -> Env Expr -> [Reactor] -> [Either Expr Expr] -> (Env Expr, [Reactor], [Either Expr Expr])

collect [] env reactors results = (env, reactors, results)

collect ((List [Symbol "display", expr]):rest) env reactors results =
    let
        result = case eval (IEnv catchException) env expr id of
            -- TODO This is less than fantastic. Should we have a dedicated error Expr?
            e@(List [(Symbol "uncaught-exception"), expr]) -> Left expr
            other -> Right other
    in
        collect rest env reactors (result:results)
    where
        catchException expr = List [(Symbol "uncaught-exception"), expr]

collect ((List [Symbol "assert", expr]):rest) env reactors results =
    case eval (IEnv stop) env expr id of
        Boolean False ->
            error ("assertion failed: " ++ show expr)
        _ ->
            collect rest env reactors results

collect ((List [Symbol "require", sym@(Symbol s)]):rest) env reactors results =
    case Env.find s env of
        Nothing ->
            error ("assertion failed: (bound? " ++ show sym ++ ")")
        _ ->
            collect rest env reactors results

collect ((List [Symbol "define", sym@(Symbol s), expr]):rest) env reactors results =
    case Env.find s env of
        Just _ ->
            error ("symbol already defined: " ++ show sym)
        Nothing ->
            let
                result = eval (IEnv stop) env expr id
            in
                collect rest (Env.insert s result env) reactors results

collect ((List [Symbol "define-if-absent", sym@(Symbol s), expr]):rest) env reactors results =
    case Env.find s env of
        Just _ ->
            collect rest env reactors results
        Nothing ->
            let
                result = eval (IEnv stop) env expr id
            in
                collect rest (Env.insert s result env) reactors results

collect ((List [Symbol "reactor", facExpr, stateExpr, bodyExpr]):rest) env reactors results =
    let
        state = eval (IEnv stop) env stateExpr id
        body = eval (IEnv stop) env bodyExpr id
        newReactor = Reactor{ rid=(fromIntegral $ length reactors), env=env, state=state, body=body }
    in
        collect rest env (newReactor:reactors) results

collect (topExpr:rest) env reactors results =
    error ("illegal top-level form: " ++ show topExpr)
