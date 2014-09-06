module Piece.I (
    I,
    grid,
) where


import Data.Function
import Grid
import New
import Piece.Factory
import Presence
import Rotate


newtype I = I { unI :: [Grid Presence] }


instance Rotate I where
    rotate Clockwise (I gs) = I $ drop 1 gs
    rotate CounterClockwise (I gs) = I $ drop 3 gs


instance New I where
    new = I $ cycle rotations


grid :: I -> Grid Presence
grid = head . unI


rotations :: [Grid Presence]
rotations = map mkPiece [
    [ "....",
      "OOOO",
      "....",
      "...." ],

    [ "..O.",
      "..O.",
      "..O.",
      "..O." ],

    [ "....",
      "....",
      "OOOO",
      "...." ],

    [ ".O..",
      ".O..",
      ".O..",
      ".O.." ]]






