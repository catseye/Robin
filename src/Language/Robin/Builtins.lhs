> module Language.Robin.Builtins where

> import qualified Language.Robin.Env as Env
> import Language.Robin.Expr
> import Language.Robin.Eval

Robin Builtins
==============

Note, these are functions which are built-in to the Robin reference
intepreter, for performance, but they are *not* intrinsic to the
Robin language.  (See Intrinsics.lhs for those.)

> robinHead i env (List [expr]) cc = do
>     eval i env expr (\x ->
>         assertList i x (\val ->
>             case val of
>                 List (a:_) -> cc a
>                 other -> raise i (errMsg "expected-list" other)))
> robinHead i env other cc = raise i (errMsg "illegal-arguments" other)

> robinTail i env (List [expr]) cc = do
>     eval i env expr (\x ->
>         assertList i x (\val ->
>             case val of
>                 List (_:b) -> cc (List b)
>                 other -> raise i (errMsg "expected-list" other)))
> robinTail i env other cc = raise i (errMsg "illegal-arguments" other)

> robinPrepend i env (List [e1, e2]) cc = do
>     eval i env e1 (\x1 -> eval i env e2 (\val ->
>             case val of
>                 List x2 -> cc $ List (x1:x2)
>                 other -> raise i (errMsg "expected-list" other)))
> robinPrepend i env other cc = raise i (errMsg "illegal-arguments" other)

> equalP i env (List [e1, e2]) cc = do
>     eval i env e1 (\x1 -> eval i env e2 (\x2 -> cc $ Boolean (x1 == x2)))
> equalP i env other cc = raise i (errMsg "illegal-arguments" other)

> predP pred i env (List [expr]) cc = do
>     eval i env expr (\x -> cc $ Boolean $ pred x)
> predP pred i env other cc = raise i (errMsg "illegal-arguments" other)

> symbolP = predP isSymbol
> listP = predP isList
> macroP = predP isMacro
> numberP = predP isNumber

> robinSubtract i env (List [xexpr, yexpr]) cc = do
>     eval i env xexpr (\x ->
>         assertNumber i x (\(Number xv) ->
>             eval i env yexpr (\y ->
>                 assertNumber i y (\(Number yv) ->
>                     cc (Number (xv - yv))))))
> robinSubtract i env other cc = raise i (errMsg "illegal-arguments" other)

> robinSign i env (List [expr]) cc = do
>     eval i env expr (\x ->
>         assertNumber i x (\(Number xv) ->
>             cc $ Number $ sign xv))
> robinSign i env other cc = raise i (errMsg "illegal-arguments" other)

> sign x = if x == 0 then 0 else if x < 0 then -1 else 1

> robinIf i env (List [test, texpr, fexpr]) cc = do
>     eval i env test (\x ->
>         assertBoolean i x (\(Boolean b) ->
>             case b of
>                 True -> eval i env texpr cc
>                 False -> eval i env fexpr cc))
> robinIf i env other cc = raise i (errMsg "illegal-arguments" other)

> robinEval i env (List [envlist, form]) cc = do
>     eval i env envlist (\newEnv ->
>         eval i env form (\body -> do
>             eval i newEnv body cc))
> robinEval i env other cc = raise i (errMsg "illegal-arguments" other)

> robinMacro i env (List [args@(List [(Symbol selfS), (Symbol argsS), (Symbol envS)]), body]) cc = do
>     cc $ Macro env args body
> robinMacro i env other cc = raise i (errMsg "illegal-arguments" other)

> robinRaise i env (List [expr]) cc =
>     eval i env expr (\v -> raise i v)
> robinRaise i env other cc = raise i (errMsg "illegal-arguments" other)

> robinCatch i env (List [id@(Symbol _), handler, body]) cc =
>     let
>         handlerContinuation = (\errvalue ->
>             eval i (Env.insert id errvalue env) handler cc)
>         i' = setExceptionHandler handlerContinuation i
>     in
>         eval i' env body cc
> robinCatch i env other cc = raise i (errMsg "illegal-arguments" other)

SMALL...

This implementation of the `small` package is non-normative.
See the relevant files in `stdlib` for normative definitions.

> union (List []) env = env
> union (List (binding:rest)) env =
>     append (List [binding]) (union (List rest) env)

> literal i env (List (expr:_)) cc =
>     cc expr
> literal i env other cc = raise i (errMsg "illegal-arguments" other)

> evalAll i env [] acc cc =
>     cc $ List $ reverse acc
> evalAll i env (head:tail) acc cc =
>     eval i env head (\value ->
>         evalAll i env tail (value:acc) cc)

> robinList i env (List exprs) cc =
>     evalAll i env exprs [] cc

> robinEnv i env (List _) cc =
>   cc env

> choose i env (List [(List [(Symbol "else"), branch])]) cc =
>     eval i env branch cc
> choose i env (List ((List [test, branch]):rest)) cc = do
>     eval i env test (\x ->
>         case x of
>             Boolean True ->
>                 eval i env branch cc
>             Boolean False ->
>                 choose i env (List rest) cc)
> choose i env other cc = raise i (errMsg "illegal-arguments" other)

> bind i env (List [name@(Symbol _), expr, body]) cc =
>     eval i env expr (\value ->
>         eval i (Env.insert name value env) body cc)
> bind i env other cc = raise i (errMsg "illegal-arguments" other)

> robinLet i env (List ((List bindings):body:_)) cc =
>     bindAll bindings env i (\env' ->
>         eval i env' body cc)
>   where
>     bindAll [] env ienv cc =
>         cc env
>     bindAll (List (name@(Symbol _):sexpr:_):rest) env ienv cc =
>         eval ienv env sexpr (\value ->
>             bindAll rest (Env.insert name value env) ienv cc)
>     bindAll (other:rest) env ienv cc =
>         raise ienv (errMsg "illegal-binding" other)
> robinLet i env other cc = raise i (errMsg "illegal-arguments" other)

> --       formals actuals origActuals env i cc
> evalArgs [] [] _ _ _ cc = do
>     cc Env.empty
> evalArgs (formal@(Symbol _):formals) (actual:actuals) origActuals env i cc = do
>     eval i env actual (\value ->
>         evalArgs formals actuals origActuals env i (\rest ->
>             cc $ Env.insert formal value rest))
> evalArgs _ _ origActuals _ i cc = do
>     raise i (errMsg "illegal-arguments" (List origActuals))

> robinBindArgs i env (List [(List formals), givenArgs, givenEnv, body]) cc = do
>     eval i env givenArgs (\(List actuals) ->
>         eval i env givenEnv (\outerEnv ->
>             evalArgs formals actuals actuals outerEnv i (\argEnv ->
>                 eval i (union argEnv env) body cc)))
> robinBindArgs i env other cc = raise i (errMsg "illegal-arguments" other)

WHATEVER...

> robinFun i closedEnv (List [(List formals), body]) cc = do
>     cc $ Intrinsic "<lambda>" fun
>   where
>     fun i env (List actuals) cc = do
>         evalArgs formals actuals actuals env i (\argEnv ->
>             eval i (union argEnv closedEnv) body cc)
>     evalArgs [] [] _ _ _ cc = do
>         cc Env.empty
>     evalArgs (formal@(Symbol _):formals) (actual:actuals) origActuals env i cc = do
>         eval i env actual (\value ->
>             evalArgs formals actuals origActuals env i (\rest ->
>                 cc $ Env.insert formal value rest))
>     evalArgs _ _ origActuals _ i cc = do
>         raise i (errMsg "illegal-arguments" (List origActuals))
> robinFun i env other cc = raise i (errMsg "illegal-arguments" other)

> booleanP = predP isBoolean

...
THE TABLE
...

> robinBuiltins = Env.fromList $ map (\(name,bif) -> (name, Intrinsic name bif))
>       [
>         ("@head",     robinHead),
>         ("@tail",     robinTail),
>         ("@prepend",  robinPrepend),
>         ("@list?",    listP),
>         ("@symbol?",  symbolP),
>         ("@macro?",   macroP),
>         ("@number?",  numberP),
>         ("@equal?",   equalP),
>         ("@subtract", robinSubtract),
>         ("@sign",     robinSign),
>         ("@macro",    robinMacro),
>         ("@eval",     robinEval),
>         ("@if",       robinIf),
>         ("@raise",    robinRaise),
>         ("@catch",    robinCatch),

>         ("literal",   literal),
>         ("list",      robinList),
>         ("bind",      bind),
>         ("env",       robinEnv),
>         ("let",       robinLet),
>         ("choose",    choose),
>         ("bind-args", robinBindArgs),
>         --("boolean?",  booleanP),
>         ("fun",       robinFun),

>         ("head",      robinHead),
>         ("tail",      robinTail),
>         ("prepend",   robinPrepend),
>         ("list?",     listP),
>         ("symbol?",   symbolP),
>         ("macro?",    macroP),
>         ("number?",   numberP),
>         ("equal?",    equalP),
>         ("subtract",  robinSubtract),
>         ("sign",      robinSign),
>         ("macro",     robinMacro),
>         ("eval",      robinEval),
>         ("if",        robinIf),
>         ("raise",     robinRaise),
>         ("catch",     robinCatch)
>       ]
