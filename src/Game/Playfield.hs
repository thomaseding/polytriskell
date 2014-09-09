module Game.Playfield (
) where


import Data.Grid (Width, Height, XLoc, YLoc, Grid, mkGrid, toList, dimensions)
import qualified Data.Grid as Grid
import Data.Function (on)
import Data.Function.Pointless ((.:))
import Data.Presence (Presence(..))
import Game.Tromino (Tromino, grid, metadata)


data Cell a
    = Empty
    | Occupied a
    deriving (Show, Eq, Ord)


type Pos = (XLoc, YLoc)


-- Coord info: (0, 0) is the top left corner of the playfield
newtype Playfield a = Playfield { unPlayfield :: Grid (Cell a) }


mkPlayfield :: Width -> Height -> Playfield a
mkPlayfield w h = Playfield $ mkGrid w h Empty


toPresence :: Cell a -> Presence
toPresence = \case
    Empty -> NotPresent
    Occupied _ -> Present


toCell :: a -> Presence -> Cell a
toCell x = \case
    NotPresent -> Empty
    Present -> Occupied x


extractGrid :: Pos -> Width -> Height -> Playfield a -> Grid (Cell a)
extractGrid = undefined


coverGrid :: Pos -> Grid (Cell a) -> Playfield a -> Playfield a
coverGrid = undefined


addTromino :: Pos -> Tromino a -> Playfield a -> Maybe (Playfield a)
addTromino = withTromino (not .: occupied) add
    where
        occupied Present (Occupied _) = True
        occupied _ _ = False
        --
        add Empty = Nothing
        add occupied = Just occupied


removeTromino :: Pos -> Tromino a -> Playfield a -> Playfield a
removeTromino p t f = case removeTromino' p t f of
    Nothing -> error "Game.removeTromino: Internal logic error."
    Just f' -> f'


removeTromino' :: Pos -> Tromino a -> Playfield a -> Maybe (Playfield a)
removeTromino' = withTromino always remove
    where
        always _ _ = True
        --
        remove Empty = Nothing
        remove occupied = Just Empty


withTromino :: (Presence -> Cell b -> Bool) -> (Cell a -> Maybe (Cell b)) -> Pos -> Tromino a -> Playfield b -> Maybe (Playfield b)
withTromino pred mask pos tromino field = case canOverlay pred trominoGrid existingGrid of
    False -> Nothing
    True -> Just $ coverGrid pos (overlay mask trominoGrid' existingGrid) field
    where
        (w, h) = dimensions trominoGrid
        existingGrid = extractGrid pos w h field
        trominoGrid = grid tromino
        trominoGrid' = fmap (toCell $ metadata tromino) trominoGrid


overlay :: (a -> Maybe b) -> Grid a -> Grid b -> Grid b
overlay mask = Grid.zipWith $ \x y -> case mask x of
    Nothing -> y
    Just y' -> y'


canOverlay :: (a -> b -> Bool) -> Grid a -> Grid b -> Bool
canOverlay pred g1 g2 = and $ zipWith pred (toCells g1) (toCells g2)
    where
        toCells :: Grid c -> [c]
        toCells = concat . toList











