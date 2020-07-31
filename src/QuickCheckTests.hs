module QuickCheckTests where

import Test.QuickCheck

import Data.Int

import Language.Robin.Expr
import Language.Robin.Env (Env, mergeEnvs, fromList, find, insert, empty)
import Language.Robin.Eval (eval)
import Language.Robin.Parser (parseToplevel, parseExpr)
import Language.Robin.Intrinsics (robinIntrinsics)
import Language.Robin.Builtins (robinBuiltins)
import qualified Language.Robin.CmdLine as CmdLine


stdEval env expr = eval env expr id


instance Arbitrary Expr where
    --
    -- Note that it's important that we narrow down how many subexpressions each expression
    -- has, otherwise we generate a lot of infinitely large expressions, which don't help.
    -- This is modelled after the technique shown here: https://stackoverflow.com/a/15959889
    --
    arbitrary = sized arbExpr where
        arbExpr :: Int -> Gen Expr
        arbExpr 0 = oneof [
                            Symbol <$> arbitrary,
                            Boolean <$> arbitrary,
                            Number <$> arbitrary
                          ]
        arbExpr n = do
          (Positive m) <- arbitrary
          let n' = n `div` (m + 1)
          oneof [
                   Abort <$> (arbExpr n'),
                   List <$> (arbExprList n'),
                   Macro <$> (arbExpr n') <*> (arbExpr n') <*> (arbExpr n')
                ]

        arbExprList :: Int -> Gen [Expr]
        arbExprList 0 = do
            return []
        arbExprList n = do
            (Positive m) <- arbitrary
            let n' = n `div` (m + 1)
            head <- arbExpr n'
            tail <- arbExprList n'
            return (head:tail)


-- TODO: establish some arbitrary newtypes too, like RobinEnv, and RobinList, which
--       are restricted in the set of terms they will generate.

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
-- The following should be true for any expression e:
-- (equal? (literal e) (literal e))
--
propEqual :: Env -> Expr -> Bool
propEqual env e =
    stdEval env expr == Boolean True
    where
        expr = List [Symbol "equal?", List [Symbol "literal", e], List [Symbol "literal", e]]


--
-- The following should be true for any expr e and list l:
-- (tail (prepend e l)) is a list
--
propList :: Env -> Expr -> Expr -> Property
propList env e l =
    isList l ==> (isList (stdEval env expr))
    where
        expr = List [Symbol "tail", List [Symbol "prepend", List [Symbol "literal", e], List [Symbol "literal", l]]]

--
-- The following should be true for any equivalent applicable expressions e1 and e2 and any list l:
-- (apply (prepend e1 l)) == (apply (prepend e2 l))
--

propEquivApply :: Env -> Expr -> Expr -> Expr -> Property
propEquivApply env e1 e2 l =
    isList l ==> (r1 == r2)
    where
        r1 = stdEval env (append (List [e1]) l)
        r2 = stdEval env (append (List [e1]) l)

--
-- ========================================================
--


testBuiltins = do
    (env, secondaryDefs) <- CmdLine.loadEnv "pkg/stdlib.robin" (mergeEnvs robinIntrinsics robinBuiltins)
    quickCheck (propEnv env)
    quickCheck (propDel env)
    quickCheck (propExt env)
    quickCheck (propList env)
    quickCheck (propEqual env)


testNoBuiltins = do
    (noBuiltinsEnv, _) <- CmdLine.loadEnv "pkg/stdlib.robin" robinIntrinsics
    quickCheck (propGt noBuiltinsEnv)
    quickCheck (propLt noBuiltinsEnv)


testSecondaryDefEnv (List []) env = return ()
testSecondaryDefEnv (List (List [Symbol name, secondaryDef]:rest)) env = do
    let Just primaryDef = find name env
    putStrLn $ "Comparing multiple definitions of " ++ name ++ "..."
    quickCheckWith stdArgs{ maxSuccess=1000 } (propEquivApply primaryDef secondaryDef)
    testSecondaryDefEnv (List rest) env


testSecondaryDefs = do
    (env, secondaryDefs) <- CmdLine.loadEnv "pkg/stdlib.robin" (mergeEnvs robinIntrinsics robinBuiltins)
    testSecondaryDefEnv secondaryDefs env


main = do
    testBuiltins
    testNoBuiltins
    testSecondaryDefs
