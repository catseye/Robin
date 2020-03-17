module Language.Robin.TopLevel where

import Prelude (show, id, fromIntegral, length, ($), (++), Bool(False), Maybe(Just, Nothing))

import Language.Robin.Expr
import Language.Robin.Env (Env, find, insert, empty)
import Language.Robin.Eval
import qualified Language.Robin.Reactor as Reactor


data World = World {
               env :: Env,
               secondaryDefs :: Env,
               reactors :: [Reactor.Reactor],
               results :: [Expr]
             }

assertionFailed expr =
    List [Symbol "assertion-failed", expr]

illegalTopLevel expr =
    List [Symbol "illegal-toplevel", expr]

initialWorld env =
    World{ env=env, reactors=[], results=[], secondaryDefs=empty }

destructureWorld world =
    (env world, reactors world, results world)


collect :: [Expr] -> World -> World

collect [] result = result

collect ((List [Symbol "display", expr]):rest) world@World{ env=env, results=results } =
    collect rest world{ results=((eval env expr id):results) }

collect ((List [Symbol "assert", expr]):rest) world@World{ env=env, results=results } =
    case eval env expr id of
        Abort expr ->
            world{ results=((Abort expr):results) }
        Boolean False ->
            world{ results=((Abort $ assertionFailed expr):results) }
        _ ->
            collect rest world

collect ((List [Symbol "require", sym@(Symbol s)]):rest) world@World{ env=env, results=results } =
    case find s env of
        Nothing ->
            world{ results=((Abort $ assertionFailed (List [Symbol "bound?", sym])):results) }
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
    world{ results=((Abort $ illegalTopLevel expr):results) }
