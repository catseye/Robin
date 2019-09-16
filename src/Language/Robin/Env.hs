module Language.Robin.Env where

import qualified Data.Map.Strict as Map

--
-- An environment is an alist which associates symbols with
-- values (arbitrary S-expressions).
--

data Env a = Env (Map.Map String a) deriving (Show, Ord, Eq)

empty :: Env a
empty = Env (Map.empty)

insert :: String -> a -> Env a -> Env a
insert sym value (Env map) = Env (Map.insert sym value map)

find :: String -> Env a -> Maybe a
find sym (Env map) = Map.lookup sym map

fromList :: [(String,a)] -> Env a
fromList pairs = Env (Map.fromList pairs)

toList :: Env a -> [(String,a)]
toList (Env map) = Map.toList map

mergeEnvs :: Env a -> Env a -> Env a
mergeEnvs (Env a) (Env b) = Env (Map.union a b)
