> module Robin.Expr where

> import Data.Char
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
>           | Boolean Bool
>           | Number (Ratio Integer)
>           | Pid ThreadId (Chan Expr)
>           | Macro Expr Expr Expr
>           | Builtin String Bif
>           | List [Expr]
>           | Metadata Expr Expr

Equality ignores metadata for now.  That's too deep a question for
me to think about right now.

> instance Eq Expr where
>     (Symbol x) == (Symbol y)         = x == y
>     (Boolean x) == (Boolean y)       = x == y
>     (Number x) == (Number y)         = x == y
>     (Pid x _) == (Pid y _)           = x == y
>     (Macro _ _ _) == (Macro _ _ _)   = False
>     (Builtin x _) == (Builtin y _)   = x == y
>     (List x) == (List y)             = x == y
>     (Metadata _ x) == (Metadata _ y) = x == y
>     (Metadata _ x) == y              = x == y
>     x == (Metadata _ y)              = x == y
>     _ == _                           = False

> instance Show Expr where
>     show (Symbol s)       = s
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
>     show (Metadata _ x)   = show x
>     show (List exprs)     = "(" ++ (showl exprs) ++ ")"

> showl [] = ""
> showl [expr] = show expr
> showl (expr:exprs) = (show expr) ++ " " ++ (showl exprs)

Helpers
-------

> append (List x) (List y) =
>     List (x ++ y)

Metadata Helpers
----------------

> hasMetadata metadata (Metadata m x)
>     | m == metadata = True
>     | otherwise     = hasMetadata metadata x
> hasMetadata _ _ = False

> stripMetadata (Metadata _ x) =
>     stripMetadata x
> stripMetadata x =
>     x

Predicates
----------

> isSymbol (Symbol _) = True
> isSymbol _          = False

> isBoolean (Boolean _) = True
> isBoolean _           = False

> isNumber (Number _) = True
> isNumber _          = False

> isList (List _) = True
> isList _        = False

> isMacro (Macro _ _ _) = True
> isMacro (Builtin _ _) = True
> isMacro _             = False
