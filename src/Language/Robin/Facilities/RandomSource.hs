module Language.Robin.Facilities.RandomSource where

import System.Random

import Language.Robin.Expr
import Language.Robin.Facilities


init :: IO Facility
init = do
    return Facility{ threadId=Nothing, handler=handleEvent, waiter=nullWaiter }


handleEvent :: FacilityHandler
handleEvent (List [Symbol "obtain-random-u16", payload]) = do
    v <- randomRIO (0, 65535)
    return $ [List [Symbol "random-u16", Number v]]
handleEvent _ = return []
