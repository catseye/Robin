module Language.Robin.Expr where

import Data.Char
import Data.Int

--
-- An _intrinsic_ is an object which behaves much like a macro, but is implemented
-- intrinsically (it cannot be (non-meta-circularly) defined in Robin itself.)
--

type Intrinsic = IEnv Expr -> Expr -> Expr -> (Expr -> Expr) -> Expr
--            internal-env    env     args    continuation      result

data Expr = Symbol String
          | Boolean Bool
          | Number Int32
          | Macro Expr Expr Expr
          | Intrinsic String Intrinsic
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
    show (List exprs)          = "(" ++ (showl exprs) ++ ")"

showl [] = ""
showl [expr] = show expr
showl (expr:exprs) = (show expr) ++ " " ++ (showl exprs)

--
-- Helpers
--

append (List x) (List y) =
    List (x ++ y)

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
