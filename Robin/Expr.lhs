> module Robin.Expr where

> import Data.Ratio

> import Control.Concurrent (ThreadId)

> import Robin.IEnv
> import Robin.Chan

Definitions
===========

A "bif" is a "built-in function" -- an acronym borrowed from Erlang,
though somewhat regrettably, as it's quite lacking as a name.

> type Bif = Expr -> IEnv Expr -> Expr -> (Expr -> IO Expr) -> IO Expr

> data Expr = Symbol String
>           | Null
>           | Boolean Bool
>           | Number (Ratio Integer)
>           | Pid ThreadId (Chan Expr)
>           | Macro Expr Expr Expr
>           | Builtin String Bif
>           | Pair Expr Expr

> instance Eq Expr where
>     (Symbol x) == (Symbol y)       = x == y
>     Null == Null                   = True
>     (Boolean x) == (Boolean y)     = x == y
>     (Number x) == (Number y)       = x == y
>     (Pid x _) == (Pid y _)         = x == y
>     (Macro _ _ _) == (Macro _ _ _) = False
>     (Builtin x _) == (Builtin y _) = x == y
>     (Pair x1 x2) == (Pair y1 y2)   = (x1 == y1) && (x2 == y2)
>     _ == _                         = False

> instance Show Expr where
>     show (Symbol s)       = s
>     show Null             = "()"
>     show (Boolean True)   = "#t"
>     show (Boolean False)  = "#f"
>     show (Number n)       = if
>                                 denominator n == 1
>                               then
>                                 show $ numerator n
>                               else
>                                 ((show $ numerator n) ++
>                                  "/" ++ (show $ denominator n))
>     show (Pid t c)        = "(pid " ++ (show t) ++ ")"
>     show (Macro env args body) = ("(macro " ++ (show args) ++
>                                   " " ++ (show body) ++ ")")
>     show (Builtin name _) = "(builtin " ++ name ++ ")"
>     show e@(Pair _ _)     = "(" ++ (showl e)

> showl Null = ")"
> showl (Pair a Null) = (show a) ++ ")"
> showl (Pair a b) = (show a) ++ " " ++ (showl b)
> showl other = ". " ++ (show other) ++ ")"

Helper Functions
----------------

A helper function to make Pair lists from Haskell lists.

> robinizeList [] last =
>     last
> robinizeList (x:xs) last =
>     Pair x (robinizeList xs last)

Predicates
----------

> isSymbol (Symbol _) = True
> isSymbol _          = False

> isPair (Pair _ _) = True
> isPair _          = False

> isBoolean (Boolean _) = True
> isBoolean _           = False

> isNumber (Number _) = True
> isNumber _          = False

> isList (Pair _ tail) = isList tail
> isList Null          = True
> isList _             = False

> isMacro (Macro _ _ _) = True
> isMacro (Builtin _ _) = True
> isMacro _             = False
