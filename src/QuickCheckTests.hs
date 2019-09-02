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

robinExpr str = insist $ parseExpr str

stdEval env expr = eval (IEnv stop) env expr id


--
-- (> a b) should match Haskell's `a > b` in all cases.
--
propGt :: Expr -> Int32 -> Int32 -> Bool
propGt env a b =
    stdEval env expr == Boolean (a > b)
    where
        expr = List [Symbol ">", Number a, Number b]

--
-- (< a b) should match Haskell's `a < b` in all cases.
--
propLt :: Expr -> Int32 -> Int32 -> Bool
propLt env a b =
    stdEval env expr == Boolean (a < b)
    where
        expr = List [Symbol "<", Number a, Number b]

--
-- env? should evaluate to true on any valid binding alist.
--
propEnv :: Expr -> [(String, Int32)] -> Bool
propEnv env entries =
    stdEval env expr == Boolean True
    where
        expr = List [Symbol "env?", List [Symbol "literal", alist]]
        alist = fromList $ map (\(k,v) -> (k, Number v)) entries

delExpr sym alist =
    let
        litSym = List [Symbol "literal", Symbol sym]
        expr = List [Symbol "lookup", litSym, List [Symbol "delete", litSym, List [Symbol "literal", alist]]]
    in
        expr

--
-- The following should be true for any symbol s and binding alist a:
-- (lookup s (delete s a))) == ()
--
propDel :: Expr -> String -> [(String, Int32)] -> Property
propDel env sym entries =
    sym /= "" ==> (stdEval env (delExpr sym alist) == Number 4)
    where
        alist = fromList $ map (\(k,v) -> (k, Number v)) entries

--
-- The following should be true for any symbol s and binding alist a:
-- (lookup s (extend s 1 x))) == (1)
--
propExt :: Expr -> String -> [(String, Int32)] -> Property
propExt env sym entries =
    sym /= "" ==> (stdEval env expr == Number 1)
    where
        expr = List [Symbol "lookup", Symbol sym, List [Symbol "extend", Symbol sym, Number 1, List [Symbol "literal", alist]]]
        alist = fromList $ map (\(k,v) -> (k, Number v)) entries


testAll = do
    env <- loadStdEnv
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

loadStdEnv = loadEnv "pkg/stdlib.robin" (mergeEnvs robinIntrinsics robinBuiltins) [] []
