module Language.Robin.Facilities where

import Language.Robin.Expr

type Event = Either String [Expr]
type FacilityHandler = Expr -> IO [Expr]
type WaitForEvents = IO Event

data Facility = Facility {
                  handler :: FacilityHandler,
                  waiter :: WaitForEvents
                }

nullWaiter :: WaitForEvents
nullWaiter = return $ Right []
