module Language.Robin.Facilities where

import Language.Robin.Expr

type FacilityHandler = Expr -> IO [Expr]
type WaitForEvents = IO (Either String [Expr])
