{-# LANGUAGE DeriveFunctor #-}

module Game.Playfield (
    Playfield,
    mkPlayfield,
    addTetromino,
    removeTetromino,
    getRow,
    putRow,
    clearRow,
    dropRow,
) where


import Data.Grid (Dimensions, Index, Grid)
import qualified Data.Grid as Grid
import Data.Function.Pointless ((.:))
import Data.Presence (Presence(..))
import Data.Set (Set)
import qualified Data.Set as Set
import Game.Tetromino (Tetromino, getGrid, getMetadata)
import Prelude hiding (pred)


data Cell a
    = Empty
    | Occupied a
    deriving (Show, Eq, Ord)


instance Functor Cell where
    fmap f = \case
        Empty -> Empty
        Occupied x -> Occupied $ f x


type CellGrid a = Grid (Cell a)


-- Coord info: (0, 0) is the top left corner of the playfield
newtype Playfield a = Playfield { unPlayfield :: CellGrid a }
    deriving (Functor)


lift :: (CellGrid a -> CellGrid a) -> (Playfield a -> Playfield a)
lift f = Playfield . f . unPlayfield


mkPlayfield :: Dimensions -> Playfield a
mkPlayfield dim = Playfield $ Grid.mkGrid dim Empty


toCell :: a -> Presence -> Cell a
toCell x = \case
    NotPresent -> Empty
    Present -> Occupied x


toPresence :: Cell a -> Presence
toPresence = \case
    Empty -> NotPresent
    Occupied _ -> Present


addTetromino :: Index -> Tetromino a -> Playfield a -> Maybe (Playfield a)
addTetromino = withTetromino (not .: colliding) add
    where
        colliding Present (Occupied _) = True
        colliding _ _ = False
        --
        add Empty = Nothing
        add occupied = Just occupied


removeTetromino :: Index -> Tetromino a -> Playfield a -> Playfield a
removeTetromino idx t f = case removeTetromino' idx t f of
    Nothing -> error "Game.removeTetromino: Internal logic error."
    Just f' -> f'


removeTetromino' :: Index -> Tetromino a -> Playfield a -> Maybe (Playfield a)
removeTetromino' = withTetromino always remove
    where
        always _ _ = True
        --
        remove Empty = Nothing
        remove (Occupied _) = Just Empty


withTetromino :: (Presence -> Cell b -> Bool) -> (Cell a -> Maybe (Cell b)) -> Index -> Tetromino a -> Playfield b -> Maybe (Playfield b)
withTetromino pred mask offset tetromino = mergeGrid pred' mask offset tetrominoGrid
    where
        tetrominoGrid = fmap (toCell $ getMetadata tetromino) $ getGrid tetromino
        pred' = pred . toPresence


mergeGrid :: (Cell a -> Cell b -> Bool) -> (Cell a -> Maybe (Cell b)) -> Index -> CellGrid a -> Playfield b -> Maybe (Playfield b)
mergeGrid pred mask offset grid field = case allowChange of
    False -> Nothing
    True -> Just $ Playfield fieldGrid'
    where
        dim = Grid.dimensions grid
        fieldGrid = unPlayfield field
        existingGrid = Grid.subGrid offset dim fieldGrid
        allowChange = Grid.canOverlay pred offset grid existingGrid
        fieldGrid' = Grid.overlayBy1 mask offset grid existingGrid


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






















