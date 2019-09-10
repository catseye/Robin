module Language.Robin.Facilities.RandomSource where

import Data.Int
import System.Random

import Language.Robin.Expr
import Language.Robin.Facilities


init :: IO Facility
init = return Facility{ handler=handleEvent, waiter=nullWaiter }


handleEvent :: FacilityHandler
handleEvent (List [Symbol "obtain-random-u16", payload]) = do
    v <- randomRIO (0, 65535) :: IO Integer
    return $ [List [Symbol "random-u16", Number (fromIntegral v)]]
handleEvent _ = return []
