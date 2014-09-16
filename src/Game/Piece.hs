{-# LANGUAGE MultiParamTypeClasses #-}

module Game.Piece (
    Piece(..),
) where


import Data.Cell (Cell)
import Data.Grid (Grid, Index)
import Data.Rotate (RotateDir)


type CellGrid a = Grid (Cell a)


class Piece p where
    getGrid :: p a -> CellGrid a
    rotate :: RotateDir -> p a -> p a
    overlayBy2 :: (Piece p) => (a -> b -> Cell b) -> Index -> CellGrid a -> p b -> p b



