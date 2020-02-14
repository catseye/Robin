module Language.Robin.TopLevel (collect) where

import Language.Robin.Expr
import Language.Robin.Eval
import Language.Robin.Reactor


collect :: [Expr] -> Env -> [Reactor] -> [Either Expr Expr] -> (Env, [Reactor], [Either Expr Expr])

collect [] env reactors results = (env, reactors, results)

collect ((List [Symbol "display", expr]):rest) env reactors results =
    let
        catchException env expr k = List [(Symbol "uncaught-exception"), expr]
        env' = setExceptionHandler (Intrinsic "(exception-handler)" catchException) env
        result = case eval env' expr id of
            (List [(Symbol "uncaught-exception"), expr]) -> Left expr
            other -> Right other
    in
        collect rest env reactors (result:results)

collect ((List [Symbol "assert", expr]):rest) env reactors results =
    case eval env expr id of
        Boolean False ->
            error ("assertion failed: " ++ show expr)
        _ ->
            collect rest env reactors results

collect ((List [Symbol "require", sym@(Symbol s)]):rest) env reactors results =
    case find s env of
        Nothing ->
            error ("assertion failed: (bound? " ++ show sym ++ ")")
        _ ->
            collect rest env reactors results

collect ((List [Symbol "define", sym@(Symbol s), expr]):rest) env reactors results =
    case find s env of
        Just _ ->
            error ("symbol already defined: " ++ show sym)
        Nothing ->
            let
                result = eval env expr id
            in
                collect rest (insert s result env) reactors results

collect ((List [Symbol "define-if-absent", sym@(Symbol s), expr]):rest) env reactors results =
    case find s env of
        Just _ ->
            collect rest env reactors results
        Nothing ->
            let
                result = eval env expr id
            in
                collect rest (insert s result env) reactors results

collect ((List [Symbol "reactor", facExpr, stateExpr, bodyExpr]):rest) env reactors results =
    let
        catchException env expr k = List [(Symbol "uncaught-exception"), expr]
        env' = setExceptionHandler (Intrinsic "(exception-handler)" catchException) env
        state = eval env' stateExpr id
        body = eval env' bodyExpr id
        newReactor = Reactor{ rid=(fromIntegral $ length reactors), env=env', state=state, body=body }
    in
        collect rest env (newReactor:reactors) results

collect (topExpr:rest) env reactors results =
    error ("illegal top-level form: " ++ show topExpr)
