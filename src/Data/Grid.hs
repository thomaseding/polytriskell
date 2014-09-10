module Data.Grid (
    Index,
    Dimensions,
    Grid,
    mkGrid,
    fromLists,
    dimensions,
    put,
    get,
    subGrid,
    zipWith,
    overlayBy1,
    overlayBy2,
    canOverlay,
) where


import Data.Maybe (fromJust)
import Math.Geometry.Grid.Square (RectSquareGrid, rectSquareGrid)
import qualified Math.Geometry.GridMap as GM
import Math.Geometry.GridMap.Lazy (LGridMap, lazyGridMap)
import Prelude hiding (lookup, zipWith)


type Index = (Int, Int)
type Dimensions = (Int, Int)
type GMap a = LGridMap RectSquareGrid a


data Grid a = Grid Dimensions (GMap a)


instance Functor Grid where
    fmap = lift . fmap


lift :: (GMap a -> GMap b) -> (Grid a -> Grid b)
lift f (Grid dim gm) = Grid dim $ f gm


getMap :: Grid a -> GMap a
getMap (Grid _ gm) = gm


mkGrid :: Dimensions -> a -> Grid a
mkGrid dim = fromList dim . repeat


fromList :: Dimensions -> [a] -> Grid a
fromList dim xs = Grid dim gm
    where
        g = uncurry rectSquareGrid dim
        gm = lazyGridMap g xs


fromLists :: [[a]] -> Grid a
fromLists xss = fromList dim xs
    where
        dim = (w, h)
        w = length $ head xss
        h = length xss
        xs = concat xss


dimensions :: Grid a -> Dimensions
dimensions (Grid dim _) = dim


unionDim :: Dimensions -> Dimensions -> Dimensions
unionDim (w1, h1) (w2, h2) = (max w1 w2, max h1 h2)


inDim :: Index -> Dimensions -> Bool
(x, y) `inDim` (w, h) = x < w && y < h


dimIndices :: Dimensions -> [Index]
dimIndices (w, h) = [(x, y) | x <- [0 .. w - 1], y <- [0 .. h - 1]]


adjust :: (a -> a) -> Index -> Grid a -> Grid a
adjust f idx = lift $ GM.adjust f idx


put :: a -> Index -> Grid a -> Grid a
put x = adjust $ const x


get :: Index -> Grid a -> a
get idx = fromJust . lookup idx


lookup :: Index -> Grid a -> Maybe a
lookup idx (Grid dim gm) = case idx `inDim` dim of
    False -> Nothing
    True -> GM.lookup idx gm


zipWith :: (a -> b -> c) -> Grid a -> Grid b -> Grid c
zipWith f = zipWithTotal $ \(Just x) (Just y) -> f x y


zipWithTotal :: (Maybe a -> Maybe b -> c) -> Grid a -> Grid b -> Grid c
zipWithTotal f gridA gridB = fromList dimC $ map g indicesC
    where
        dimA = dimensions gridA
        dimB = dimensions gridB
        dimC = unionDim dimA dimB
        indicesC = dimIndices dimC
        g idx = let
            a = lookup idx gridA
            b = lookup idx gridB
            in f a b
        

addIndices :: Index -> Index -> Index
addIndices (x, y) (x', y') = (x + x', y + y')


subGrid :: Index -> Dimensions -> Grid a -> Grid a
subGrid idx subDim grid = fromList subDim subVals
    where
        subIndices = map (addIndices idx) $ dimIndices subDim
        subVals = map (flip get grid) subIndices


overlayBy1 :: (a -> Maybe b) -> Index -> Grid a -> Grid b -> Grid b
overlayBy1 f = overlayBy2 $ \x _ -> f x


overlayBy2 :: (a -> b -> Maybe b) -> Index -> Grid a -> Grid b -> Grid b
overlayBy2 f offset gridA gridB = foldr g gridB $ GM.toList $ getMap gridA
    where
        g (idx, x) grid = let
            idx' = addIndices offset idx
            y = get idx' gridB
            in case f x y of
                Nothing -> grid
                Just y' -> put y' idx' grid


canOverlay :: (a -> b -> Bool) -> Index -> Grid a -> Grid b -> Bool
canOverlay pred offset gridA gridB = foldr f True $ GM.toList $ getMap gridA
    where
        f (idx, x) success = case success of
            False -> False
            True -> let
                idx' = addIndices offset idx
                y = get idx' gridB
                in pred x y









