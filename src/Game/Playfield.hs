module Game.Playfield (
) where


import Data.Grid (Dimensions, Index, Grid)
import qualified Data.Grid as Grid
import Data.Function (on)
import Data.Function.Pointless ((.:))
import Data.Presence (Presence(..))
import Game.Tetromino (Tetromino, grid, metadata)


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


extractGrid :: Index -> Dimensions -> Playfield a -> Grid (Cell a)
extractGrid = undefined


placeGrid :: Index -> Grid (Cell a) -> Playfield a -> Playfield a
placeGrid = undefined


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
        remove occupied = Just Empty


withTetromino :: (Presence -> Cell b -> Bool) -> (Cell a -> Maybe (Cell b)) -> Index -> Tetromino a -> Playfield b -> Maybe (Playfield b)
withTetromino pred mask offset tetromino field = case allowChange of
    False -> Nothing
    True -> Just $ Playfield newGrid
    where
        dim = Grid.dimensions tetrominoGrid
        existingGrid = extractGrid offset dim field
        tetrominoGrid = grid tetromino
        tetrominoGrid' = fmap (toCell $ metadata tetromino) tetrominoGrid
        allowChange = Grid.canOverlay pred offset tetrominoGrid existingGrid
        newGrid = Grid.overlayBy1 mask offset tetrominoGrid' existingGrid













