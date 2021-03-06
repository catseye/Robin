module Main where

import Haste
import Haste.DOM
import Haste.Events

import Language.Robin.Env (mergeEnvs)
import Language.Robin.Parser (parseToplevel)
import Language.Robin.Intrinsics (robinIntrinsics)
import Language.Robin.Builtins (robinBuiltins)
import Language.Robin.TopLevel (initialWorld, destructureWorld, collect)


main = withElems ["prog", "result", "run-button"] driver

driver [progElem, resultElem, runButtonElem] = do
    onEvent runButtonElem Click $ \_ -> execute
    where
        execute = do
            Just program <- getValue progElem
            case parseToplevel program of
                Right topExprs -> do
                    let env = (mergeEnvs robinIntrinsics robinBuiltins)
                    let world = collect topExprs (initialWorld env)
                    let (env', reactors, results) = destructureWorld world
                    setProp resultElem "textContent" $ showResults results
                Left problem -> do
                    setProp resultElem "textContent" $ show $ problem
        showResults results =
            (foldl (\a x -> x ++ "\n" ++ a) "" (map (show) results))
