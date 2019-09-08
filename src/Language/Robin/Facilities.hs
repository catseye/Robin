module Language.Robin.Facilities where

import Language.Robin.Expr

type Event = Either String [Expr]
type FacilityHandler = Expr -> IO [Expr]
type WaitForEvents = IO Event
