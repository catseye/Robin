module QuickCheckTests where

import Test.QuickCheck

import Data.Int

import System.IO
import System.Environment
import System.Exit

import Language.Robin.Expr
import Language.Robin.Env (Env, mergeEnvs, fromList, find)
import Language.Robin.Eval (eval)
import Language.Robin.Parser (parseToplevel, parseExpr)
import Language.Robin.Intrinsics (robinIntrinsics)
import Language.Robin.Builtins (robinBuiltins)
import qualified Language.Robin.TopLevel as TopLevel


stdEval env expr = eval env expr id

robinIntrinsic name = case find name robinIntrinsics of
    Just intrinsic -> pure intrinsic

instance Arbitrary Expr where
    arbitrary = oneof [
                  Symbol <$> arbitrary,
                  Boolean <$> arbitrary,
                  Number <$> arbitrary,
                  Abort <$> arbitrary,
                  List <$> arbitrary,
                  Macro <$> arbitrary <*> arbitrary <*> arbitrary,
                  robinIntrinsic "head",
                  robinIntrinsic "tail",
                  robinIntrinsic "prepend",
                  robinIntrinsic "list?",
                  robinIntrinsic "symbol?",
                  robinIntrinsic "macro?",
                  robinIntrinsic "number?",
                  robinIntrinsic "equal?",
                  robinIntrinsic "subtract",
                  robinIntrinsic "sign",
                  robinIntrinsic "macro",
                  robinIntrinsic "eval",
                  robinIntrinsic "if",
                  robinIntrinsic "abort",
                  robinIntrinsic "recover"
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


testAll = do
    env <- loadEnv "pkg/stdlib.robin" (mergeEnvs robinIntrinsics robinBuiltins) [] []
    noBuiltinsEnv <- loadEnv "pkg/stdlib.robin" robinIntrinsics [] []
    quickCheck (propGt noBuiltinsEnv)
    quickCheck (propLt noBuiltinsEnv)
    quickCheck (propEnv env)
    quickCheck (propDel env)
    quickCheck (propExt env)

loadEnv filename env reactors results = do
    program <- readFile filename
    case parseToplevel program of
        Right topExprs -> do
            (env', reactors', results') <- return $ TopLevel.collect topExprs env reactors results
            return env'
        Left problem -> do
            hPutStr stderr (show problem)
            exitWith $ ExitFailure 1
