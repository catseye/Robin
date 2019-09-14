module Language.Robin.Expr where

import Data.Char
import Data.Int

import qualified Language.Robin.Env as Env

--
-- An _evaluable_ is a Haskell object which behaves like a Robin macro.
-- It describes builtins (which includes intrinsics), and also happens
-- (perhaps unsurprisingly?) to be the type of the evaluator function.
--

type Evaluable = IEnv Expr -> Env.Env Expr -> Expr -> (Expr -> Expr) -> Expr
--            internal-env    env             args    continuation      result

data Expr = Symbol String
          | Boolean Bool
          | Number Int32
          | Macro (Env.Env Expr) Expr Expr
          | Intrinsic String Evaluable
          | List [Expr]

instance Eq Expr where
    (Symbol x) == (Symbol y)           = x == y
    (Boolean x) == (Boolean y)         = x == y
    (Number x) == (Number y)           = x == y
    (Macro _ _ _) == (Macro _ _ _)     = False
    (Intrinsic x _) == (Intrinsic y _) = x == y
    (List x) == (List y)               = x == y
    _ == _                             = False

instance Show Expr where
    show (Symbol s)            = s
    show (Boolean True)        = "#t"
    show (Boolean False)       = "#f"
    show (Number n)            = show n
    show (Macro env args body) = ("(macro " ++ (show args) ++
                                  " " ++ (show body) ++ ")")
    show (Intrinsic name _)    = name
    show (List exprs)          = "(" ++ (showl exprs) ++ ")" where
                                     showl [] = ""
                                     showl [expr] = show expr
                                     showl (expr:exprs) = (show expr) ++ " " ++ (showl exprs)

--
-- Helpers
--

append (List x) (List y) =
    List (x ++ y)

exprToEnv :: Expr -> Either (String, Expr) (Env.Env Expr)
exprToEnv (List []) = Right Env.empty
exprToEnv (List (first:rest)) =
    case first of
        List [Symbol s, value] ->
            case exprToEnv (List rest) of
                Right remainder -> Right (Env.insert s value remainder)
                other -> other
        List [other, _] ->
            Left ("expected-symbol", other)
        other ->
            Left ("expected-env-entry", other)
exprToEnv other = Left ("expected-env-alist", other)

envToExpr :: Env.Env Expr -> Expr
envToExpr (Env.Env []) = List []
envToExpr (Env.Env ((s, value):rest)) =
    append (List [List [Symbol s, value]]) (envToExpr (Env.Env rest))

--
-- Predicates
--

isSymbol (Symbol _) = True
isSymbol _          = False

isBoolean (Boolean _) = True
isBoolean _           = False

isNumber (Number _) = True
isNumber _          = False

isList (List _) = True
isList _        = False

isMacro (Macro _ _ _)   = True
isMacro (Intrinsic _ _) = True
isMacro _               = False

--
-- The _internal environment_ is the evaluation environment for Robin which is entirely
-- internal; Robin programs cannot see or modify it directly.  Here
-- we keep things like the continuation which is the current exception handler.
--

data IEnv t = IEnv (t -> t)

stop expr =
    error ("uncaught exception: " ++ show expr)

getExceptionHandler (IEnv handler) = handler
setExceptionHandler handler (IEnv _) = (IEnv handler)
