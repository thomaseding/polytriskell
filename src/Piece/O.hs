module Piece.O (
    O,
    grid,
) where


import Data.Function
import Grid
import New
import Piece.Factory
import Presence
import Rotate


newtype O = O { unO :: [Grid Presence] }


instance Rotate O where
    rotate Clockwise = id
    rotate CounterClockwise = id


instance New O where
    new = O $ cycle rotations


grid :: O -> Grid Presence
grid = head . unO


rotations :: [Grid Presence]
rotations = mkRotations $
    [ "OO",
      "OO" ]





