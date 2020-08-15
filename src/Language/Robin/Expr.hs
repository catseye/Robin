module Language.Robin.Expr where

import Data.Char
import Data.Int

--
-- An _evaluable_ is a Haskell object which implements a Robin operator.
-- It describes both builtins (which includes intrinsics) and user-defined
-- functions, macros, and fexprs, and also happens (perhaps unsurprisingly?)
-- to be the type of the evaluator function.
--

type Evaluable = Expr -> Expr -> (Expr -> Expr) -> Expr
--               env     args    continuation      result

--
-- Basic expressions in Robin.  These may be evaluated, or they may be
-- the result of evaluating an expression.
--

data Expr = Symbol String
          | Boolean Bool
          | Number Int32
          | Operator String Evaluable
          | List [Expr]
          | Abort Expr

instance Eq Expr where
    (Symbol x) == (Symbol y)             = x == y
    (Boolean x) == (Boolean y)           = x == y
    (Number x) == (Number y)             = x == y
    (Operator x _) == (Operator y _)     = x == y
    (List x) == (List y)                 = x == y
    (Abort x) == (Abort y)               = x == y
    _ == _                               = False

instance Show Expr where
    show (Symbol s)            = s
    show (Boolean True)        = "#t"
    show (Boolean False)       = "#f"
    show (Number n)            = show n
    show (Operator name _)     = name
    show (Abort e)             = "(abort " ++ (show e) ++ ")"
    show (List exprs)          = "(" ++ (showl exprs) ++ ")" where
                                     showl [] = ""
                                     showl [expr] = show expr
                                     showl (expr:exprs) = (show expr) ++ " " ++ (showl exprs)

--
-- Helpers
--

append (List x) (List y) =
    List (x ++ y)

abortVal msg term =
    Abort (List [(Symbol msg), term])

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

isAbort (Abort _) = True
isAbort _         = False

isOperator (Operator _ _) = True
isOperator _              = False
