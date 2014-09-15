{-# LANGUAGE MultiParamTypeClasses #-}

module Game.Piece (
    Piece(..),
) where


import Data.Cell (Cell)
import Data.Grid (Grid, Index)
import Data.Rotate (Rotatable)


type CellGrid a = Grid (Cell a)


class Rotatable (p a) => Piece p a where
    getGrid :: p a -> CellGrid a
    overlayBy2 :: (Piece p a, Piece p b) => (a -> b -> Cell b) -> Index -> CellGrid a -> p b -> p b



