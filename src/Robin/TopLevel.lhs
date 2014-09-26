> module Robin.TopLevel where

> import Robin.Expr
> import qualified Robin.Env as Env
> import Robin.Eval
> import Robin.Reactor

Top-Level S-Expressions
-----------------------

> execTopExprs env reactors [] = return (env, reactors)

> execTopExprs env reactors ((List [Symbol "display", expr]):rest) = do
>     result <- eval (IEnv stop) env expr (\x -> do return x)
>     putStrLn $ show result
>     execTopExprs env reactors rest

> execTopExprs env reactors ((List [Symbol "define", name@(Symbol _), expr]):rest) = do
>     case Env.find name env of
>         Just _ -> do
>             error ("symbol already defined: " ++ show name)
>         Nothing -> do
>             result <- eval (IEnv stop) env expr (\x -> do return x)
>             execTopExprs (Env.insert name result env) reactors rest

> execTopExprs env reactors ((List [Symbol "reactor", facExpr, stateExpr, bodyExpr]):rest) = do
>     state <- eval (IEnv stop) env stateExpr (\x -> do return x)
>     body <- eval (IEnv stop) env bodyExpr (\x -> do return x)
>     execTopExprs env ((Reactor env state body):reactors) rest

> execTopExprs env reactors (topExpr:rest) = do
>     error ("illegal top-level form: " ++ show topExpr)
