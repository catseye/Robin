module Language.Robin.Facilities.RandomSource where

import System.Random

import Language.Robin.Expr
import Language.Robin.Facilities


handleRandomSourceEvent :: FacilityHandler
handleRandomSourceEvent (List [Symbol "obtain-random-u16", payload]) = do
    v <- randomRIO (0, 65535)
    return $ [List [Symbol "random-u16", Number v]]
handleRandomSourceEvent _ = return []
