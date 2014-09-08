module Piece.T (
    T,
    grid,
) where


import Data.Function
import Grid
import New
import Piece.Factory
import Presence
import Rotate


newtype T = T { unT :: [Grid Presence] }


instance Rotate T where
    rotate Clockwise (T gs) = T $ drop 1 gs
    rotate CounterClockwise (T gs) = T $ drop 3 gs


instance New T where
    new = T $ cycle rotations


grid :: T -> Grid Presence
grid = head . unT


rotations :: [Grid Presence]
rotations = mkRotations $
    [ ".O.",
      "OOO",
      "..." ]





