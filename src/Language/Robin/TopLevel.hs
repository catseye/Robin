module Language.Robin.TopLevel where

import Prelude (show, id, fromIntegral, length, ($), (++), Bool(False), Maybe(Just, Nothing), Either(Left, Right))

import Language.Robin.Expr
import Language.Robin.Env (Env, find, insert, empty)
import Language.Robin.Eval
import qualified Language.Robin.Reactor as Reactor


data World = World {
               env :: Env,
               secondaryDefs :: Env,
               reactors :: [Reactor.Reactor],
               results :: [Either Expr Expr]
             }

initialWorld env =
    World{ env=env, reactors=[], results=[], secondaryDefs=empty }

destructureWorld world =
    (env world, reactors world, results world)


collect :: [Expr] -> World -> World

collect [] result = result

collect ((List [Symbol "display", expr]):rest) world@World{ env=env, results=results } =
    let
        result = case eval env expr id of
            Abort expr -> Left (Abort expr)
            other -> Right other
    in
        collect rest world{ results=(result:results) }

collect ((List [Symbol "assert", expr]):rest) world@World{ env=env, results=results } =
    case eval env expr id of
        Abort expr ->
            world{ results=((Left (Abort expr)):results) }
        Boolean False ->
            world{ results=((Left (Abort (Symbol ("assertion failed: " ++ show expr)))):results) }
        _ ->
            collect rest world

collect ((List [Symbol "require", sym@(Symbol s)]):rest) world@World{ env=env, results=results } =
    case find s env of
        Nothing ->
            world{ results=((Left (Abort (List [Symbol "bound?", sym]))):results) }
        _ ->
            collect rest world

collect ((List [Symbol "define", sym@(Symbol s), expr]):rest) world@World{ env=env, secondaryDefs=secondaryDefs } =
    case find s env of
        Just _ ->
            let
                result = eval env expr id
            in
                collect rest world{ secondaryDefs=(insert s result secondaryDefs) }
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

collect (expr:rest) world@World{ results=results } =
    world{ results=((Left (Abort (Symbol ("illegal top-level form: " ++ show expr)))):results) }
