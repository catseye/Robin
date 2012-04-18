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

> eval (List []) ienv s@(Symbol _) cc =
>     raise ienv (errMsg "unbound-identifier" s)
> eval (List (b@(List [id@(Symbol _), value]):env)) ienv s@(Symbol _) cc
>     | id == s   = cc value
>     | otherwise = eval (List env) ienv s cc
> eval (List ((List (other:_)):env)) ienv s@(Symbol _) cc =
>     raise ienv (errMsg "expected-symbol" other)
> eval (List (head:tail)) ienv s@(Symbol _) cc =
>     raise ienv (errMsg "expected-env-entry" head)
> eval env ienv s@(Symbol _) cc =
>     raise ienv (errMsg "expected-env-alist" env)

Evaluating a list means we must make several evaluations.  We
evaluate the head to obtain something to apply (which must be a
macro, built-in or not.)  We then apply the body of the macro,
passing it the tail of the list.

> eval env ienv (List (applierExpr:actuals)) cc = do
>     eval env ienv applierExpr (\applier ->
>         case (stripMetadata applier) of
>             m@(Macro _ _ body) -> do
>                 eval (makeMacroEnv env (List actuals) m) ienv body cc
>             b@(Builtin _ fun) -> do
>                 fun env ienv (List actuals) cc
>             other ->
>                 raise ienv (errMsg "inapplicable-object" other))

Evaluating something with metadata is the same as evaluating the same
thing without metadata.

> eval env ienv (Metadata _ e) cc =
>     eval env ienv e cc

Everything else just evaluates to itself.  Continue the current
continuation with that value.

> eval env ienv e cc = do
>     cc e

Helper functions
----------------

> errMsg msg term =
>     List [(Symbol msg), term]

> makeMacroEnv env actuals m@(Macro closedEnv argList _)  =
>     let
>         (List [argSelf@(Symbol _), argFormal@(Symbol _),
>                envFormal@(Symbol _)]) = argList
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

Assertions
----------

> assert pred msg ienv expr cc =
>     let
>         expr' = stripMetadata expr
>     in
>         case pred expr' of
>             True -> cc expr'
>             False -> raise ienv (errMsg msg expr')

> assertSymbol = assert (isSymbol) "expected-symbol"
> assertBoolean = assert (isBoolean) "expected-boolean"
> assertList = assert (isList) "expected-list"
> assertNumber = assert (isNumber) "expected-number"
> assertMacro = assert (isMacro) "expected-macro"
