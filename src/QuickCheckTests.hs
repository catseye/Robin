module QuickCheckTests where

import Test.QuickCheck

import Data.Int

import System.IO
import System.Environment
import System.Exit

import Language.Robin.Expr
import Language.Robin.Env (mergeEnvs, fromList)
import Language.Robin.Eval (eval)
import Language.Robin.Parser (parseToplevel, parseExpr)
import Language.Robin.Intrinsics (robinIntrinsics)
import Language.Robin.Builtins (robinBuiltins)
import qualified Language.Robin.TopLevel as TopLevel


insist (Right x) = x

--
-- (> a b) should match Haskell's `a > b` in all cases.
--
propGt :: Expr -> Int32 -> Int32 -> Bool
propGt env a b =
    eval (IEnv stop) env expr id == Boolean (a > b)
    where
        expr = List [Symbol ">", Number a, Number b]

--
-- (< a b) should match Haskell's `a < b` in all cases.
--
propLt :: Expr -> Int32 -> Int32 -> Bool
propLt env a b =
    eval (IEnv stop) env expr id == Boolean (a < b)
    where
        expr = List [Symbol "<", Number a, Number b]

--
-- env? should evaluate to true on any valid binding alist.
--
propEnv :: Expr -> [(String, Int32)] -> Bool
propEnv env entries =
    eval (IEnv stop) env expr id == Boolean True
    where
        expr = List [Symbol "env?", List [Symbol "literal", alist]]
        alist = fromList $ map (\(k,v) -> (k, Number v)) entries

--
-- The following should be true for any symbol s and binding alist a:
-- (lookup s (delete s a))) == ()
--
propDel :: Expr -> String -> [(String, Int32)] -> Bool
propDel env sym entries =
    eval (IEnv stop) env expr id == Number 4
    where
        litSym = List [Symbol "literal", Symbol sym]
        expr = List [Symbol "lookup", litSym, List [Symbol "delete", litSym, List [Symbol "literal", alist]]]
        alist = fromList $ map (\(k,v) -> (k, Number v)) entries

--
-- The following should be true for any symbol s and binding alist a:
-- (lookup s (extend s 1 x))) == (1)
--
propExt :: Expr -> String -> [(String, Int32)] -> Bool
propExt env sym entries =
    eval (IEnv stop) env expr id == Number 1
    where
        expr = List [Symbol "lookup", Symbol sym, List [Symbol "extend", Symbol sym, Number 1, List [Symbol "literal", alist]]]
        alist = fromList $ map (\(k,v) -> (k, Number v)) entries


testAll = do
    env <- loadEnv "pkg/stdlib.robin" (mergeEnvs robinIntrinsics robinBuiltins) [] []
    quickCheck (propGt env)
    quickCheck (propLt env)
    quickCheck (propEnv env)
    --quickCheck (propDel env)
    --quickCheck (propExt env)


loadEnv filename env reactors results = do
    program <- readFile filename
    case parseToplevel program of
        Right topExprs -> do
            (env', reactors', results') <- return $ TopLevel.collect topExprs env reactors results
            return env'
        Left problem -> do
            hPutStr stderr (show problem)
            exitWith $ ExitFailure 1



