> module Robin.Eval where

> import Robin.IEnv
> import qualified Robin.Env as Env
> import Robin.Expr

Evaluator
=========

This is written in continuation-passing style.

Every evaluation function is (and takes) a continuation, which is implemented
as a function with signature:

    Expr -> IEnv -> Expr -> (Expr -> IO Expr) -> IO Expr

(This is actually the `Bif` type from `Robin.Expr`.)

The first argument is the Robin environment, which is directly visible
(and modifiable, during `eval`) by Robin program.  The second is the
internal context, which contains things like the exception handler, etc.

When evaluating a symbol, look it up in the environment to obtain a
value.  Then continue the current continuation with that value.

> eval :: Bif

> eval Null ienv s@(Symbol _) cc =
>     raise ienv (Pair (Symbol "unbound-identifier") s)
> eval (Pair b@(Pair id@(Symbol _) value) env) ienv s@(Symbol _) cc
>     | id == s   = cc value
>     | otherwise = eval env ienv s cc
> eval (Pair (Pair other _) env) ienv s@(Symbol _) cc =
>     raise ienv (Pair (Symbol "expected-symbol") other)
> eval (Pair head tail) ienv s@(Symbol _) cc =
>     raise ienv (Pair (Symbol "expected-pair") head)
> eval env ienv s@(Symbol _) cc =
>     raise ienv (Pair (Symbol "expected-pair") env)

Evaluating a pair means we must make several evaluations.  We
evaluate the head to obtain something to apply (which must be a
macro, built-in or not.)  We then apply the body of the macro,
passing it the tail of the pair.

> eval env ienv (Pair applierExpr actuals) cc = do
>     eval env ienv applierExpr (\applier ->
>         case (stripMetadata applier) of
>             m@(Macro _ _ body) -> do
>                 trace ienv m
>                 eval (makeMacroEnv env actuals (stripMetadata m)) ienv body cc
>             b@(Builtin _ fun) -> do
>                 trace ienv b
>                 fun env ienv actuals cc
>             other ->
>                 raise ienv (Pair (Symbol "inapplicable-object") other))

Evaluating something with metadata is the same as evaluating the same
thing without metadata.

> eval env ienv (Metadata _ e) cc =
>     eval env ienv e cc

Everything else just evaluates to itself.  Continue the current
continuation with that value.

> eval env ienv e cc = do
>     trace ienv e
>     cc e

Helper function
---------------

> makeMacroEnv env actuals m@(Macro closedEnv argList _)  =
>     let
>         (Pair argSelf@(Symbol _) (Pair argFormal@(Symbol _)
>          (Pair envFormal@(Symbol _) Null))) = argList
>         newEnv = Env.insert argSelf m closedEnv
>         newEnv' = Env.insert argFormal actuals newEnv
>         newEnv'' = Env.insert envFormal env newEnv'
>     in
>         newEnv''

Exception Handler
-----------------

> raise :: IEnv Expr -> Expr -> IO Expr
> raise ienv expr =
>     (getExceptionHandler ienv) expr

Tracing
-------

> trace :: IEnv Expr -> Expr -> IO ()
> trace ienv expr =
>     if getTrace ienv then
>         do print (getThreadId ienv, expr)
>       else
>         return ()

Assertions
----------

> assert pred msg ienv expr cc =
>     let
>         expr' = stripMetadata expr
>     in
>         case pred expr' of
>             True -> cc expr'
>             False -> raise ienv (Pair (Symbol msg) expr')

> assertSymbol = assert (isSymbol) "expected-symbol"
> assertBoolean = assert (isBoolean) "expected-boolean"
> assertPair = assert (isPair) "expected-pair"
> assertNumber = assert (isNumber) "expected-number"
> assertMacro = assert (isMacro) "expected-macro"
