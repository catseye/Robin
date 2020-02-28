module QuickCheckTests where

import Test.QuickCheck

import Data.Int

import Language.Robin.Expr
import Language.Robin.Env (Env, mergeEnvs, fromList, find)
import Language.Robin.Eval (eval)
import Language.Robin.Parser (parseToplevel, parseExpr)
import Language.Robin.Intrinsics (robinIntrinsics)
import Language.Robin.Builtins (robinBuiltins)
import qualified Language.Robin.CmdLine as CmdLine


stdEval env expr = eval env expr id


instance Arbitrary Expr where
    arbitrary = oneof [
                  Symbol <$> arbitrary,
                  Boolean <$> arbitrary,
                  Number <$> arbitrary,
                  Abort <$> arbitrary,
                  List <$> arbitrary,
                  Macro <$> arbitrary <*> arbitrary <*> arbitrary
                ]

--
-- (gt? a b) should match Haskell's `a > b` in all cases.
--
propGt :: Env -> Int32 -> Int32 -> Bool
propGt env a b =
    stdEval env expr == Boolean (a > b)
    where
        expr = List [Symbol "gt?", Number a, Number b]

--
-- (lt? a b) should match Haskell's `a < b` in all cases.
--
propLt :: Env -> Int32 -> Int32 -> Bool
propLt env a b =
    stdEval env expr == Boolean (a < b)
    where
        expr = List [Symbol "lt?", Number a, Number b]

--
-- env? should evaluate to true on any valid binding alist.
--
propEnv :: Env -> [(String, Int32)] -> Bool
propEnv env entries =
    stdEval env expr == Boolean True
    where
        expr = List [Symbol "env?", List [Symbol "literal", alist]]
        alist = fromList $ map (\(k,v) -> (k, Number v)) entries


--
-- The following should be true for any symbol s and binding alist a:
-- (lookup s (delete s a))) == ()
--
propDel :: Env -> String -> [(String, Int32)] -> Property
propDel env sym entries =
    sym /= "" ==> (stdEval env expr == List [])
    where
        litSym = List [Symbol "literal", Symbol sym]
        expr = List [Symbol "lookup", litSym, List [Symbol "delete", litSym, List [Symbol "literal", alist]]]
        alist = fromList $ map (\(k,v) -> (k, Number v)) entries

--
-- The following should be true for any symbol s and binding alist a:
-- (lookup s (extend s 1 x))) == (1)
--
propExt :: Env -> String -> [(String, Int32)] -> Property
propExt env sym entries =
    sym /= "" ==> (stdEval env expr == List [Number 1])
    where
        litSym = List [Symbol "literal", Symbol sym]
        expr = List [Symbol "lookup", litSym, List [Symbol "extend", litSym, Number 1, List [Symbol "literal", alist]]]
        alist = fromList $ map (\(k,v) -> (k, Number v)) entries


--
-- The following should be true for any list l:
-- (equal? l l)
--
propListEq :: Env -> Expr -> Property
propListEq env l =
    isList l ==> (stdEval env expr == Boolean True)
    where
        expr = List [Symbol "equal?", List [Symbol "literal", l], List [Symbol "literal", l]]


--
-- The following should be true for any expr e and list l:
-- (tail (prepend e l)) is a list
--
propList :: Env -> Expr -> Expr -> Property
propList env e l =
    isList l ==> (isList (stdEval env expr))
    where
        expr = List [Symbol "tail", List [Symbol "prepend", List [Symbol "literal", e], List [Symbol "literal", l]]]


testAll = do
    env <- CmdLine.loadEnv "pkg/stdlib.robin" (mergeEnvs robinIntrinsics robinBuiltins)
    noBuiltinsEnv <- CmdLine.loadEnv "pkg/stdlib.robin" robinIntrinsics
    quickCheck (propGt noBuiltinsEnv)
    quickCheck (propLt noBuiltinsEnv)
    quickCheck (propEnv env)
    quickCheck (propDel env)
    quickCheck (propExt env)
    --quickCheck (propListEq env)
    quickCheck (propList env)
