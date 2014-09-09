module Game.Playfield (
) where


import Data.Grid (Width, Height, XLoc, YLoc, Grid, mkGrid, toList, dimensions)
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


add :: Pos -> Tromino a -> Playfield a -> Maybe (Playfield a)
add pos tromino field = case collides trominoGrid existingGrid of
    True -> Nothing
    False -> Just $ overlay pos trominoGrid field
    where
        (w, h) = dimensions trominoGrid
        trominoGrid = fmap (toCell $ metadata tromino) $ grid tromino
        existingGrid = extractGrid pos w h field


extractGrid :: Pos -> Width -> Height -> Playfield a -> Grid (Cell a)
extractGrid = undefined


overlay :: Pos -> Grid (Cell a) -> Playfield a -> Playfield a
overlay pos grid field = undefined
    where
        (w, h) = dimensions grid
        existingGrid = extractGrid pos w h field


canOverlay :: (a -> a -> Bool) -> Grid a -> Grid a -> Bool
canOverlay pred = and .: (zipWith pred `on` toCells)
    where
        toCells = concat . toList


collides :: Grid (Cell a) -> Grid (Cell a) -> Bool
collides = not .: canOverlay (not .: occupied)
    where
        occupied (Occupied _) (Occupied _) = True
        occupied _ _ = False









