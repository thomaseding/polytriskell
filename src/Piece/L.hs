module Piece.L (
    L,
    grid,
) where


import Data.Function
import Grid
import New
import Piece.Factory
import Presence
import Rotate


newtype L = L { unL :: [Grid Presence] }


instance Rotate L where
    rotate Clockwise (L gs) = L $ drop 1 gs
    rotate CounterClockwise (L gs) = L $ drop 3 gs


instance New L where
    new = L $ cycle rotations


grid :: L -> Grid Presence
grid = head . unL


rotations :: [Grid Presence]
rotations = mkRotations $
    [ "..O",
      "OOO",
      "..." ]






