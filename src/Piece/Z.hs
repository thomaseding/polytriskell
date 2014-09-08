module Piece.Z (
    Z,
    grid,
) where


import Data.Function
import Grid
import New
import Piece.Factory
import Presence
import Rotate


newtype Z = Z { unZ :: [Grid Presence] }


instance Rotate Z where
    rotate Clockwise (Z gs) = Z $ drop 1 gs
    rotate CounterClockwise (Z gs) = Z $ drop 3 gs


instance New Z where
    new = Z $ cycle rotations


grid :: Z -> Grid Presence
grid = head . unZ


rotations :: [Grid Presence]
rotations = mkRotations $
    [ "OO.",
      ".OO",
      "..." ]





