module QuickCheckTests where

import Test.QuickCheck

import Data.Int

import System.IO
import System.Environment
import System.Exit

import Language.Robin.Expr
import Language.Robin.Env (mergeEnvs)
import Language.Robin.Eval (eval)
import Language.Robin.Parser (parseRobin, parseRobinExpr)
import Language.Robin.Intrinsics (robinIntrinsics)
import Language.Robin.Builtins (robinBuiltins)
import qualified Language.Robin.TopLevel as TopLevel


propGt :: Expr -> Int32 -> Int32 -> Bool
propGt env a b =
    eval (IEnv stop) env expr id == Boolean (a > b)
    where
        expr = List [Symbol ">", Number a, Number b]


propLt :: Expr -> Int32 -> Int32 -> Bool
propLt env a b =
    eval (IEnv stop) env expr id == Boolean (a < b)
    where
        expr = List [Symbol "<", Number a, Number b]


testAll = do
    env <- loadEnv "pkg/stdlib.robin" (mergeEnvs robinIntrinsics robinBuiltins) [] []
    quickCheck (propGt env)
    quickCheck (propLt env)


loadEnv filename env reactors results = do
    program <- readFile filename
    case parseRobin program of
        Right topExprs -> do
            (env', reactors', results') <- return $ TopLevel.collect topExprs env reactors results
            return env'
        Left problem -> do
            hPutStr stderr (show problem)
            exitWith $ ExitFailure 1



