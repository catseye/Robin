module Language.Robin.TopLevel where

import Prelude (error, show, id, fromIntegral, length, ($), (++), Bool(False), Maybe(Just, Nothing), Either(Left, Right))

import Language.Robin.Expr
import Language.Robin.Env (Env, find, insert)
import Language.Robin.Eval
import qualified Language.Robin.Reactor as Reactor


data Outcome = Outcome {
                 env :: Env,
                 reactors :: [Reactor.Reactor],
                 results :: [Either Expr Expr]
               }


collect :: [Expr] -> Outcome -> Outcome

collect [] result = result

collect ((List [Symbol "display", expr]):rest) outcome@Outcome{ env=env, results=results } =
    let
        result = case eval env expr id of
            Abort expr -> Left expr
            other -> Right other
    in
        collect rest outcome{ results=(result:results) }

collect ((List [Symbol "assert", expr]):rest) outcome@Outcome{ env=env } =
    case eval env expr id of
        Abort expr ->
            error ("uncaught exception: " ++ show expr)
        Boolean False ->
            error ("assertion failed: " ++ show expr)
        _ ->
            collect rest outcome

collect ((List [Symbol "require", sym@(Symbol s)]):rest) outcome@Outcome{ env=env } =
    case find s env of
        Nothing ->
            error ("assertion failed: (bound? " ++ show sym ++ ")")
        _ ->
            collect rest outcome

collect ((List [Symbol "define", sym@(Symbol s), expr]):rest) outcome@Outcome{ env=env } =
    case find s env of
        Just _ ->
            -- for now, take it entirely on faith that the definitions are equivalent
            collect rest outcome
        Nothing ->
            let
                result = eval env expr id
            in
                collect rest outcome{ env=(insert s result env) }

collect ((List [Symbol "reactor", facExpr, stateExpr, bodyExpr]):rest) outcome@Outcome{ env=env, reactors=reactors } =
    let
        state = eval env stateExpr id
        body = eval env bodyExpr id
        newReactor = Reactor.Reactor{ Reactor.rid=(fromIntegral $ length reactors), Reactor.env=env, Reactor.state=state, Reactor.body=body }
    in
        collect rest outcome{ reactors=(newReactor:reactors) }

collect (topExpr:rest) outcome =
    error ("illegal top-level form: " ++ show topExpr)
