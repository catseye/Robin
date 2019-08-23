module Main where

import Haste
import Haste.DOM
import Haste.Events

import Language.Robin.Env (mergeEnvs)
import Language.Robin.Parser (parseRobin)
import Language.Robin.Intrinsics (robinIntrinsics)
import Language.Robin.Builtins (robinBuiltins)
import qualified Language.Robin.TopLevel as TopLevel


main = withElems ["prog", "result", "run-button"] driver

driver [progElem, resultElem, runButtonElem] = do
    onEvent runButtonElem Click $ \_ -> execute
    where
        execute = do
            Just program <- getValue progElem
            case parseRobin program of
                Right topExprs -> do
                    let env = (mergeEnvs robinIntrinsics robinBuiltins)
                    let (env', reactors, results) = TopLevel.collect topExprs env [] []
                    setProp resultElem "textContent" $ (foldl (\a x -> x ++ "\n" ++ a) "" (map (show) results))
                Left problem -> do
                    setProp resultElem "textContent" $ show $ problem
