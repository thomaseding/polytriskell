module Game.Piece (
    Piece(..),
) where


import Data.Cell (Cell)
import Data.Grid (Grid)
import Data.Rotate (Rotatable)


class Rotatable (p a) => Piece p a where
    getGrid :: p a -> Grid (Cell a)





