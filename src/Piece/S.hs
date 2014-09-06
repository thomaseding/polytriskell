module Piece.S (
    S,
    grid,
) where


import Data.Function
import Grid
import New
import Piece.Factory
import Presence
import Rotate


newtype S = S { unS :: [Grid Presence] }


instance Rotate S where
    rotate Clockwise (S gs) = S $ drop 1 gs
    rotate CounterClockwise (S gs) = S $ drop 3 gs


instance New S where
    new = S $ cycle rotations


grid :: S -> Grid Presence
grid = head . unS


rotations :: [Grid Presence]
rotations = map mkPiece [
    [ ".OO",
      "OO.",
      "..." ],

    [ ".O.",
      ".OO",
      "..O" ],

    [ "...",
      ".OO",
      "OO." ],

    [ "O..",
      "OO.",
      ".O." ]]





