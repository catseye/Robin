module Language.Robin.Env where

import Language.Robin.Expr

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
insert s value term = abortVal "expected-env-list" term

find :: String -> Env -> Maybe Expr
find _ (List []) = Nothing
find s (List (List [Symbol t, value]:rest))
    | s == t    = Just value
    | otherwise = find s (List rest)
find s (List (_:rest)) = find s (List rest)
find _ (_) = Nothing

fromList :: [(String, Expr)] -> Env
fromList [] = empty
fromList ((s, val):rest) = insert s val $ fromList rest

mergeEnvs :: Env -> Env -> Env
mergeEnvs (List a) (List b) = (List (a ++ b))
mergeEnvs (List a) term = abortVal "expected-env-list" term
mergeEnvs term (List b) = abortVal "expected-env-list" term
