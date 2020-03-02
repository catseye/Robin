module Language.Robin.TopLevel where

import Prelude (error, show, id, fromIntegral, length, ($), (++), Bool(False), Maybe(Just, Nothing), Either(Left, Right))

import Language.Robin.Expr
import Language.Robin.Env (Env, find, insert)
import Language.Robin.Eval
import qualified Language.Robin.Reactor as Reactor


data World = World {
               env :: Env,
               reactors :: [Reactor.Reactor],
               results :: [Either Expr Expr]
             }

initialWorld env =
    World{ env=env, reactors=[], results=[] }

destructureWorld world =
    (env world, reactors world, results world)


collect :: [Expr] -> World -> World

collect [] result = result

collect ((List [Symbol "display", expr]):rest) world@World{ env=env, results=results } =
    let
        result = case eval env expr id of
            Abort expr -> Left expr
            other -> Right other
    in
        collect rest world{ results=(result:results) }

collect ((List [Symbol "assert", expr]):rest) world@World{ env=env } =
    case eval env expr id of
        Abort expr ->
            error ("uncaught exception: " ++ show expr)
        Boolean False ->
            error ("assertion failed: " ++ show expr)
        _ ->
            collect rest world

collect ((List [Symbol "require", sym@(Symbol s)]):rest) world@World{ env=env } =
    case find s env of
        Nothing ->
            error ("assertion failed: (bound? " ++ show sym ++ ")")
        _ ->
            collect rest world

collect ((List [Symbol "define", sym@(Symbol s), expr]):rest) world@World{ env=env } =
    case find s env of
        Just _ ->
            -- for now, take it entirely on faith that the definitions are equivalent
            -- note we can't just collect all definitions into env naively, because we
            -- don't want to end up using a slow definition indiscriminately.
            collect rest world
        Nothing ->
            let
                result = eval env expr id
            in
                collect rest world{ env=(insert s result env) }

collect ((List [Symbol "reactor", facExpr, stateExpr, bodyExpr]):rest) world@World{ env=env, reactors=reactors } =
    let
        state = eval env stateExpr id
        body = eval env bodyExpr id
        newReactor = Reactor.Reactor{ Reactor.rid=(fromIntegral $ length reactors), Reactor.env=env, Reactor.state=state, Reactor.body=body }
    in
        collect rest world{ reactors=(newReactor:reactors) }

collect (topExpr:rest) world =
    error ("illegal top-level form: " ++ show topExpr)
