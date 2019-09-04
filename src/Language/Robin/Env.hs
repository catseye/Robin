module Language.Robin.Env where

import Language.Robin.Expr

--
-- An environment is an alist which associates symbols with
-- values (arbitrary S-expressions).
--

empty :: Expr
empty = List []

insert :: Expr -> Expr -> Expr -> Expr
insert s@(Symbol _) value env =
    append (List [List [s, value]]) env

find :: Expr -> Expr -> Maybe Expr
find s@(Symbol _) (List []) = Nothing
find s@(Symbol _) (List (List [first, value]:rest))
    | s == first   = Just value
    | otherwise    = find s (List rest)

fromList :: [(String,Expr)] -> Expr
fromList [] =
    List []
fromList ((id, val):xs) =
    append (List [List [(Symbol id), val]]) (fromList xs)

mergeEnvs :: Expr -> Expr -> Expr
mergeEnvs (List a) (List b) = List (a ++ b)
