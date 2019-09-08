module Language.Robin.Facilities where

import Control.Concurrent (ThreadId)

import Language.Robin.Expr

type Event = Either String [Expr]
type FacilityHandler = Expr -> IO [Expr]
type WaitForEvents = IO Event

data Facility = Facility {
                  threadId :: Maybe ThreadId,
                  handler :: FacilityHandler,
                  waiter :: WaitForEvents
                }

nullWaiter :: WaitForEvents
nullWaiter = return $ Right []
