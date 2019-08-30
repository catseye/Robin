module QuickCheckTests where

import Test.QuickCheck

import Data.Int

import System.IO
import System.Environment
import System.Exit

import Language.Robin.Expr
import Language.Robin.Env (mergeEnvs, fromList)
import Language.Robin.Eval (eval)
import Language.Robin.Parser (parseRobin, parseRobinExpr)
import Language.Robin.Intrinsics (robinIntrinsics)
import Language.Robin.Builtins (robinBuiltins)
import qualified Language.Robin.TopLevel as TopLevel


-- (> a b) should match Haskell's `a > b`
propGt :: Expr -> Int32 -> Int32 -> Bool
propGt env a b =
    eval (IEnv stop) env expr id == Boolean (a > b)
    where
        expr = List [Symbol ">", Number a, Number b]

-- (< a b) should match Haskell's `a < b`
propLt :: Expr -> Int32 -> Int32 -> Bool
propLt env a b =
    eval (IEnv stop) env expr id == Boolean (a < b)
    where
        expr = List [Symbol "<", Number a, Number b]

-- The following should be true for any symbol s and alist a:
-- (lookup s (delete s a))) == ()
propDel :: Expr -> String -> [(String, Int32)] -> Bool
propDel env sym entries =
    eval (IEnv stop) env expr id == List []
    where
        expr = List [Symbol "lookup", Symbol sym, List [Symbol "delete", Symbol sym, List [Symbol "literal", alist]]]
        alist = fromList $ map (\(k,v) -> (k, Number v)) entries

-- The following should be true for any identifier i and alist x:
-- (lookup i (extend i 1 x))) == (1)
propExt :: Expr -> String -> Expr -> Bool
propExt env i x =
    eval (IEnv stop) env expr id == Number 1
    where
        expr = List [Symbol "lookup", Symbol i, List [Symbol "extend", Symbol i, Number 1, x]]


testAll = do
    env <- loadEnv "pkg/stdlib.robin" (mergeEnvs robinIntrinsics robinBuiltins) [] []
    quickCheck (propGt env)
    quickCheck (propLt env)
    --quickCheck (propDel env)
    --quickCheck (propExt env)


loadEnv filename env reactors results = do
    program <- readFile filename
    case parseRobin program of
        Right topExprs -> do
            (env', reactors', results') <- return $ TopLevel.collect topExprs env reactors results
            return env'
        Left problem -> do
            hPutStr stderr (show problem)
            exitWith $ ExitFailure 1



