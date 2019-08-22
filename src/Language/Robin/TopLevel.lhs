> module Language.Robin.TopLevel where

> import Language.Robin.Expr
> import qualified Language.Robin.Env as Env
> import Language.Robin.Eval
> import Language.Robin.Reactor

Top-Level S-Expressions
-----------------------

> execTopExprs env reactors [] = return (env, reactors)

> execTopExprs env reactors ((List [Symbol "display", expr]):rest) = do
>     result <- return $ eval (IEnv stop) env expr (\x -> x)
>     putStrLn $ show result
>     execTopExprs env reactors rest

> execTopExprs env reactors ((List [Symbol "define", name@(Symbol _), expr]):rest) = do
>     case Env.find name env of
>         Just _ -> do
>             error ("symbol already defined: " ++ show name)
>         Nothing -> do
>             result <- return $ eval (IEnv stop) env expr (\x -> x)
>             execTopExprs (Env.insert name result env) reactors rest

> execTopExprs env reactors ((List [Symbol "reactor", facExpr, stateExpr, bodyExpr]):rest) = do
>     state <- return $ eval (IEnv stop) env stateExpr (\x -> x)
>     body <- return $ eval (IEnv stop) env bodyExpr (\x -> x)
>     execTopExprs env ((Reactor env state body):reactors) rest

> execTopExprs env reactors (topExpr:rest) = do
>     error ("illegal top-level form: " ++ show topExpr)
