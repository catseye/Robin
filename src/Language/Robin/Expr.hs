module Language.Robin.Expr where

import Data.Char
import Data.Int

--
-- An _evaluable_ is a Haskell object which behaves like a Robin macro.
-- It describes builtins (which includes intrinsics), and also happens
-- (perhaps unsurprisingly?) to be the type of the evaluator function.
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
          | Macro Expr Expr Expr      -- the 1st Expr is actually an Env
          | Builtin String Evaluable
          | List [Expr]
          | Abort Expr

instance Eq Expr where
    (Symbol x) == (Symbol y)             = x == y
    (Boolean x) == (Boolean y)           = x == y
    (Number x) == (Number y)             = x == y
    (Macro e1 a1 b1) == (Macro e2 a2 b2) = e1 == e2 && a1 == a2 && b1 == b2
    (Builtin x _) == (Builtin y _)       = x == y
    (List x) == (List y)                 = x == y
    (Abort x) == (Abort y)               = x == y
    _ == _                               = False

instance Show Expr where
    show (Symbol s)            = s
    show (Boolean True)        = "#t"
    show (Boolean False)       = "#f"
    show (Number n)            = show n
    show (Macro env args body) = ("(macro " ++ (show args) ++
                                  " " ++ (show body) ++ ")")
    show (Builtin name _)      = name
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

isMacro (Macro _ _ _) = True
isMacro (Builtin _ _) = True
isMacro _             = False

isAbort (Abort _) = True
isAbort _         = False
