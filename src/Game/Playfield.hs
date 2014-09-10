module Game.Playfield (
    Playfield,
    mkPlayfield,
    addTetromino,
    removeTetromino,
    removeRow,
) where


import Data.Grid (Dimensions, Index, Grid)
import qualified Data.Grid as Grid
import Data.Function.Pointless ((.:))
import Data.Presence (Presence(..))
import Game.Tetromino (Tetromino, getGrid, getMetadata)
import Prelude hiding (pred)


data Cell a
    = Empty
    | Occupied a
    deriving (Show, Eq, Ord)


-- Coord info: (0, 0) is the top left corner of the playfield
newtype Playfield a = Playfield { unPlayfield :: Grid (Cell a) }


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


mergeGrid :: (Cell a -> Cell b -> Bool) -> (Cell a -> Maybe (Cell b)) -> Index -> Grid (Cell a) -> Playfield b -> Maybe (Playfield b)
mergeGrid pred mask offset grid field = case allowChange of
    False -> Nothing
    True -> Just $ Playfield fieldGrid'
    where
        dim = Grid.dimensions grid
        fieldGrid = unPlayfield field
        existingGrid = Grid.subGrid offset dim fieldGrid
        allowChange = Grid.canOverlay pred offset grid existingGrid
        fieldGrid' = Grid.overlayBy1 mask offset grid existingGrid


rowIndices :: Int -> Playfield a -> [Index]
rowIndices row field = [(x, row) | x <- [0 .. w - 1]]
    where
        (w, _) = Grid.dimensions $ unPlayfield field


removeRow :: Int -> Playfield a -> Playfield a
removeRow row = applyGravity . clearRow row


clearRow :: Int -> Playfield a -> Playfield a
clearRow = undefined


applyGravity :: Playfield a -> Playfield a
applyGravity = undefined











