module Game.Playfield (
) where


import Data.Grid (Width, Height, XLoc, YLoc, Grid, mkGrid, toList, dimensions)
import qualified Data.Grid as Grid
import Data.Function (on)
import Data.Function.Pointless ((.:))
import Data.Presence (Presence(..))
import Game.Tetromino (Tetromino, grid, metadata)


data Cell a
    = Empty
    | Occupied a
    deriving (Show, Eq, Ord)


type Pos = (XLoc, YLoc)


-- Coord info: (0, 0) is the top left corner of the playfield
newtype Playfield a = Playfield { unPlayfield :: Grid (Cell a) }


mkPlayfield :: Width -> Height -> Playfield a
mkPlayfield w h = Playfield $ mkGrid w h Empty


toCell :: a -> Presence -> Cell a
toCell x = \case
    NotPresent -> Empty
    Present -> Occupied x


extractGrid :: Pos -> Width -> Height -> Playfield a -> Grid (Cell a)
extractGrid = undefined


placeGrid :: Pos -> Grid (Cell a) -> Playfield a -> Playfield a
placeGrid = undefined


addTetromino :: Pos -> Tetromino a -> Playfield a -> Maybe (Playfield a)
addTetromino = withTetromino (not .: colliding) add
    where
        colliding Present (Occupied _) = True
        colliding _ _ = False
        --
        add Empty = Nothing
        add occupied = Just occupied


removeTetromino :: Pos -> Tetromino a -> Playfield a -> Playfield a
removeTetromino p t f = case removeTetromino' p t f of
    Nothing -> error "Game.removeTetromino: Internal logic error."
    Just f' -> f'


removeTetromino' :: Pos -> Tetromino a -> Playfield a -> Maybe (Playfield a)
removeTetromino' = withTetromino always remove
    where
        always _ _ = True
        --
        remove Empty = Nothing
        remove occupied = Just Empty


withTetromino :: (Presence -> Cell b -> Bool) -> (Cell a -> Maybe (Cell b)) -> Pos -> Tetromino a -> Playfield b -> Maybe (Playfield b)
withTetromino pred mask pos tetromino field = case canOverlay pred tetrominoGrid existingGrid of
    False -> Nothing
    True -> Just $ placeGrid pos (overlay mask tetrominoGrid' existingGrid) field
    where
        (w, h) = dimensions tetrominoGrid
        existingGrid = extractGrid pos w h field
        tetrominoGrid = grid tetromino
        tetrominoGrid' = fmap (toCell $ metadata tetromino) tetrominoGrid


overlay :: (a -> Maybe b) -> Grid a -> Grid b -> Grid b
overlay mask = Grid.zipWith $ \x y -> case mask x of
    Nothing -> y
    Just y' -> y'


canOverlay :: (a -> b -> Bool) -> Grid a -> Grid b -> Bool
canOverlay pred g1 g2 = and $ zipWith pred (toCells g1) (toCells g2)
    where
        toCells = concat . toList











