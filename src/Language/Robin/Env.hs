module Language.Robin.Env where

--
-- An environment is an alist which associates symbols with
-- values (arbitrary S-expressions).
--

data Env a = Env [(String, a)] deriving (Show, Ord, Eq)

empty :: Env a
empty = Env []

insert :: String -> a -> Env a -> Env a
insert s value (Env bindings) = Env ((s, value):bindings)

find :: String -> Env a -> Maybe a
find _ (Env []) = Nothing
find s (Env ((t, value):rest))
    | s == t    = Just value
    | otherwise = find s (Env rest)

fromList :: [(String,a)] -> Env a
fromList [] = empty
fromList ((s, val):rest) = insert s val $ fromList rest

mergeEnvs :: Env a -> Env a -> Env a
mergeEnvs (Env a) (Env b) = Env (a ++ b)
