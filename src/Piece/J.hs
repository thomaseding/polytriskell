module Piece.J (
    J,
    grid,
) where


import Data.Function
import Grid
import New
import Piece.Factory
import Presence
import Rotate


newtype J = J { unJ :: [Grid Presence] }


instance Rotate J where
    rotate Clockwise (J gs) = J $ drop 1 gs
    rotate CounterClockwise (J gs) = J $ drop 3 gs


instance New J where
    new = J $ cycle rotations


grid :: J -> Grid Presence
grid = head . unJ


rotations :: [Grid Presence]
rotations = map mkPiece [
    [ "O..",
      "OOO",
      "..." ],

    [ ".OO",
      ".O.",
      ".O." ],

    [ "...",
      "OOO",
      "..O" ],

    [ ".O.",
      ".O.",
      "OO." ]]





