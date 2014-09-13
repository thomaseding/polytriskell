module Data.Grid (
    Index,
    Dimensions,
    Grid,
    mkGrid,
    fromLists,
    dimensions,
    getIndices,
    put,
    get,
    subGrid,
    overlayBy1,
    overlayBy2,
    canOverlay,
) where


import Data.List (transpose)
import Data.Maybe (fromJust)
import Math.Geometry.Grid.Square (RectSquareGrid, rectSquareGrid)
import qualified Math.Geometry.Grid as G
import qualified Math.Geometry.GridMap as GM
import Math.Geometry.GridMap.Lazy (LGridMap, lazyGridMap)
import Prelude hiding (lookup, pred)


type Index = (Int, Int)
type Dimensions = (Int, Int)
type GMap a = LGridMap RectSquareGrid a


data Grid a = Grid Dimensions (GMap a)


instance Functor Grid where
    fmap = lift . fmap


instance (Show a) => Show (Grid a) where
    show (Grid dim gm) = show $ GM.toMap gm


lift :: (GMap a -> GMap b) -> (Grid a -> Grid b)
lift f (Grid dim gm) = Grid dim $ f gm


getMap :: Grid a -> GMap a
getMap (Grid _ gm) = gm


mkGrid :: Dimensions -> a -> Grid a
mkGrid dim = fromList dim . repeat


swap :: (a, b) -> (b, a)
swap (x, y) = (y, x)


fromList :: Dimensions -> [a] -> Grid a
fromList dim xs = Grid dim gm
    where
        g = uncurry rectSquareGrid $ swap dim
        gm = lazyGridMap g xs


fromLists :: [[a]] -> Grid a
fromLists xss = case isRect of
    False -> error "Data.Grid.fromLists expects a regular 2D array."
    True -> fromList dim $ concat $ transpose xss
    where
        dim = (w, h)
        w = length $ head xss
        h = length xss
        isRect = all (== w) $ map length xss


dimensions :: Grid a -> Dimensions
dimensions (Grid dim _) = dim


inDim :: Index -> Dimensions -> Bool
(x, y) `inDim` (w, h) = x < w && y < h


dimIndices :: Dimensions -> [Index]
dimIndices (w, h) = [(x, y) | x <- [0 .. w - 1], y <- [0 .. h - 1]]


getIndices :: Grid a-> [Index]
getIndices = dimIndices . dimensions


adjust :: (a -> a) -> Index -> Grid a -> Grid a
adjust f idx = lift $ GM.adjust f idx


put :: a -> Index -> Grid a -> Grid a
put x = adjust $ const x


get :: Index -> Grid a -> a
get idx grid = case lookup idx grid of
    Nothing -> error $ "Grid.get:Index=" ++ show idx ++ ",Dim=" ++ show (dimensions grid) ++ ";;;" ++ let
        Grid _ gm = grid
        in show $ G.indices $ GM.toGrid gm
    Just x -> x


lookup :: Index -> Grid a -> Maybe a
lookup idx (Grid dim gm) = case idx `inDim` dim of
    False -> Nothing
    True -> GM.lookup idx gm


addIndices :: Index -> Index -> Index
addIndices (x, y) (x', y') = (x + x', y + y')


subGrid :: Index -> Dimensions -> Grid a -> Grid a
subGrid offset subDim grid = fromList subDim subVals
    where
        subIndices = map (addIndices offset) $ dimIndices subDim
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


canOverlay :: (a -> Maybe b -> Bool) -> Index -> Grid a -> Grid b -> Bool
canOverlay pred offset gridA gridB = foldr f True $ GM.toList $ getMap gridA
    where
        f (idx, x) success = case success of
            False -> False
            True -> let
                idx' = addIndices offset idx
                my = lookup idx' gridB
                in pred x my









