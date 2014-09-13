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
) where


import Data.Cell (Cell(..))
import Data.Grid (Dimensions, Index, Grid)
import qualified Data.Grid as Grid
import Data.Function.Pointless ((.:))
import Data.Set (Set)
import qualified Data.Set as Set
import Game.Piece (Piece(..))
import Prelude hiding (pred)


type CellGrid a = Grid (Cell a)


-- Coord info: (0, 0) is the top left corner of the playfield
newtype Playfield a = Playfield { unPlayfield :: CellGrid a }
    deriving (Functor)


lift :: (CellGrid a -> CellGrid a) -> (Playfield a -> Playfield a)
lift f = Playfield . f . unPlayfield


mkPlayfield :: Dimensions -> Playfield a
mkPlayfield dim = Playfield $ Grid.mkGrid dim Empty


addPiece :: (Piece p a) => Index -> p a -> Playfield a -> Maybe (Playfield a)
addPiece = withPiece (not .: colliding) add
    where
        colliding (Occupied _) (Occupied _) = True
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


withPiece :: (Piece p a) => (Cell a -> Cell b -> Bool) -> (Cell a -> Maybe (Cell b)) -> Index -> p a -> Playfield b -> Maybe (Playfield b)
withPiece pred mask offset tetromino = mergeGrid pred mask offset tetrominoGrid
    where
        tetrominoGrid = getGrid tetromino


mergeGrid :: (Cell a -> Cell b -> Bool) -> (Cell a -> Maybe (Cell b)) -> Index -> CellGrid a -> Playfield b -> Maybe (Playfield b)
mergeGrid pred mask offset grid field = case allowChange of
    False -> Nothing
    True -> Just $ Playfield fieldGrid'
    where
        dim = Grid.dimensions grid
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
        (w, h) = Grid.dimensions grid
        cells' = take w cells
        f (cell, idx) = Grid.put cell idx


clearRow :: Int -> Playfield a -> Playfield a
clearRow row = putRow row $ repeat Empty


dropRow :: Int -> Playfield a -> Playfield a
dropRow row field = putRow (row + 1) cells $ clearRow row field
    where
        cells = getRow row field


{-
-- Let's just implement Naive gravity for now.
--
flood :: (a -> Bool) -> Index -> Grid a -> [Index]
flood p start = flood' Set.empty p [start]


flood' :: Set Index -> (a -> Bool) -> [Index] -> Grid a -> [Index]
flood' seen p pending grid = case pending of
    [] -> []
    idx : idxs -> let
        x = Grid.get idx grid
        idxs' = neighbors grid idx
        seen' = Set.insert idx seen
        in case p x of
            False -> flood' seen' p idxs grid
            True -> idx : flood' seen p (idxs' ++ idxs) grid


neighbors :: Grid a -> Index -> [Index]
neighbors grid (x, y) = let
    (w, h) = Grid.dimensions grid
    in [(x', y')
        | x' <- [x - 1 .. x + 1]
        , y' <- [y - 1 .. y + 1]
        , x' >= 0
        , y' >= 0
        , x' < w
        , y' < h
        , x' == x || y' == y
        , (x', y') /= (x, y)
        ]


type GroupId = Int


data FloodState a = Dry a | Wet a GroupId


unflood :: FloodState a -> a
unflood = \case
    Dry x -> x
    Wet x _ -> x


applyGravity :: Playfield a -> Playfield a
applyGravity = fmap unflood . dropGroups . floodCells . fmap Dry


dropGroups :: Playfield (FloodState a) -> Playfield (FloodState a)
dropGroups = undefined


floodCells :: Playfield (FloodState a) -> Playfield (FloodState a)
floodCells field = Playfield $ floodCells' [0..] idxs grid
    where
        grid = unPlayfield field
        idxs = Grid.getIndices grid


floodCells' :: [GroupId] -> [Index] -> CellGrid (FloodState a) -> CellGrid (FloodState a)
floodCells' ids pending grid = case pending of
    [] -> grid
    idx : idxs -> case Grid.get idx grid of
        Occupied (Dry x) -> let
            mkWet wetIdx grid = case Grid.get wetIdx grid of
                Occupied (Dry y) -> Grid.put (Occupied $ Wet y $ head ids) wetIdx grid
                _ -> grid   -- should never happen, but this is safe anyway
            wetIdxs = flood isDry idx grid
            in foldr mkWet grid wetIdxs
        _ -> floodCells' ids idxs grid
    where
        isDry = \case
            Occupied (Dry _) -> True
            _ -> False
-}






















