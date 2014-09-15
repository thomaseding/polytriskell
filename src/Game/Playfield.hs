{-# LANGUAGE DeriveFunctor #-}

module Game.Playfield (
    Playfield,
    mkPlayfield,
    addPiece,
    removePiece,
    getRow,
    putRow,
    clearRow,
    dropRow,
    dropRowsAbove,
) where


import Data.Cell (Cell(..))
import Data.Grid (Dimensions, Index, Grid)
import qualified Data.Grid as Grid
import Data.Function.Pointless ((.:))
import Game.Piece (Piece(..))
import Prelude hiding (pred)


type CellGrid a = Grid (Cell a)


-- Coord info: (0, 0) is the top left corner of the playfield
newtype Playfield a = Playfield { unPlayfield :: CellGrid a }
    deriving (Functor)


mkPlayfield :: Dimensions -> Playfield a
mkPlayfield dim = Playfield $ Grid.mkGrid dim Empty


addPiece :: (Piece p a) => Index -> p a -> Playfield a -> Maybe (Playfield a)
addPiece = withPiece (not .: colliding) add
    where
        colliding (Occupied _) Nothing = True
        colliding (Occupied _) (Just (Occupied _)) = True
        colliding _ _ = False
        --
        add Empty = Nothing
        add occupied = Just occupied


removePiece :: (Piece p a) => Index -> p a -> Playfield a -> Playfield a
removePiece idx t f = case removePiece' idx t f of
    Nothing -> error "Game.removePiece: Internal logic error."
    Just f' -> f'


removePiece' :: (Piece p a) => Index -> p a -> Playfield a -> Maybe (Playfield a)
removePiece' = withPiece always remove
    where
        always _ _ = True
        --
        remove Empty = Nothing
        remove (Occupied _) = Just Empty


withPiece :: (Piece p a) => (Cell a -> Maybe (Cell b) -> Bool) -> (Cell a -> Maybe (Cell b)) -> Index -> p a -> Playfield b -> Maybe (Playfield b)
withPiece pred mask offset tetromino = mergeGrid pred mask offset tetrominoGrid
    where
        tetrominoGrid = getGrid tetromino


mergeGrid :: (Cell a -> Maybe (Cell b) -> Bool) -> (Cell a -> Maybe (Cell b)) -> Index -> CellGrid a -> Playfield b -> Maybe (Playfield b)
mergeGrid pred mask offset grid field = case allowChange of
    False -> Nothing
    True -> Just $ Playfield fieldGrid'
    where
        fieldGrid = unPlayfield field
        allowChange = Grid.canOverlay pred offset grid fieldGrid
        fieldGrid' = Grid.overlayBy1 mask offset grid fieldGrid


rowIndices :: Int -> Grid a -> [Index]
rowIndices row grid = [(x, row) | x <- [0 .. w - 1]]
    where
        (w, _) = Grid.dimensions grid


getRow :: Int -> Playfield a -> [Cell a]
getRow row (Playfield grid) = map (`Grid.get` grid) $ rowIndices row grid


putRow :: Int -> [Cell a] -> Playfield a -> Playfield a
putRow row cells field = Playfield $ foldr f grid $ zip cells' $ rowIndices row grid
    where
        grid = unPlayfield field
        (w, _) = Grid.dimensions grid
        cells' = take w cells
        f (cell, idx) = Grid.put cell idx


clearRow :: Int -> Playfield a -> Playfield a
clearRow row = putRow row $ repeat Empty


dropRow :: Int -> Playfield a -> Playfield a
dropRow row field = putRow (row + 1) cells $ clearRow row field
    where
        cells = getRow row field


dropRowsAbove :: Int -> Playfield a -> Playfield a
dropRowsAbove row field = foldr dropRow field rows
    where
        rows = [0 .. row - 1]
























