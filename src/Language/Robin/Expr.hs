module Language.Robin.Expr where

import Data.Char
import Data.Int

--
-- An _evaluable_ is a Haskell object which behaves like a Robin macro.
-- It describes builtins (which includes intrinsics), and also happens
-- (perhaps unsurprisingly?) to be the type of the evaluator function.
--

type Evaluable = IEnv Expr -> Env -> Expr -> (Expr -> Expr) -> Expr
--            internal-env    env    args    continuation      result

--
-- Basic expressions in Robin.  These may be evaluated, or they may be
-- the result of evaluating an expression.
--

data Expr = Symbol String
          | Boolean Bool
          | Number Int32
          | Macro Env Expr Expr
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
-- An environment is an alist which associates symbols with
-- values (arbitrary S-expressions).
--

type Env = Expr

empty :: Env
empty = List []

insert :: String -> Expr -> Env -> Env
insert s value (List bindings) =
    let
       entry = List [Symbol s, value]
    in
       List (entry:bindings)

find :: String -> Env -> Maybe Expr
find _ (List []) = Nothing
find s (List (List [Symbol t, value]:rest))
    | s == t    = Just value
    | otherwise = find s (List rest)
find s (List (_:rest)) = find s (List rest)

fromList :: [(String, Expr)] -> Env
fromList [] = empty
fromList ((s, val):rest) = insert s val $ fromList rest

mergeEnvs :: Env -> Env -> Env
mergeEnvs (List a) (List b) = (List (a ++ b))

--
-- Helpers
--

append (List x) (List y) =
    List (x ++ y)

--
-- Given a list of pairs (two-elements lists) of symbols and values,
-- return either an Env where each symbol is associated with its value,
-- or an error message describing why the Env could not be created.
--

-- exprToEnv :: Expr -> Either (String, Expr) Env
-- exprToEnv (List []) = Right empty
-- exprToEnv (List (first:rest)) =
--     case first of
--         List [Symbol s, value] ->
--             case exprToEnv (List rest) of
--                 Right remainder -> Right (insert s value remainder)
--                 other -> other
--         List [other, _] ->
--             Left ("expected-symbol", other)
--         other ->
--             Left ("expected-env-entry", other)
-- exprToEnv other = Left ("expected-env-alist", other)


exprToEnv :: Expr -> Either (String, Expr) Env
exprToEnv env@(List _) = Right env
exprToEnv other = Left ("expected-env-alist", other)

envToExpr :: Env -> Expr
envToExpr env = env

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
