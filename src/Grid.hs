module Grid (
    Width,
    Height,
    XLoc,
    YLoc,
    Grid,
    mkGrid,
    fromList,
    toList,
    put,
    get,
) where


import Data.Array


type Width = Int
type Height = Int


type XLoc = Int
type YLoc = Int


newtype Grid a = Grid { unGrid :: [Array Int a] }
    deriving (Eq, Ord)


instance Show a => Show (Grid a) where
    show = show . toList


toList :: Grid a -> [[a]]
toList = map elems . unGrid


mkGrid :: Width -> Height -> a -> Grid a
mkGrid w h x = let
    row = listArray (0, w - 1) (repeat x)
    in Grid $ replicate h row


fromList :: [[a]] -> Grid a
fromList xss = let
    w = maximum $ map length xss
    in Grid $ map (listArray (0, w - 1)) xss


putRow :: Array Int a -> Int -> Grid a -> Grid a
putRow row n (Grid rs) = let
    leadingRs = take n rs
    trailingRs = drop (n + 1) rs
    in Grid $ leadingRs ++ [row] ++ trailingRs


getRow :: Int -> Grid a -> Array Int a
getRow n = head . drop n . unGrid


put :: a -> XLoc -> YLoc -> Grid a -> Grid a
put val x y g = let
    row = getRow y g
    row' = row // [(x, val)]
    in putRow row' y g


get :: XLoc -> YLoc -> Grid a -> a
get x y g = getRow y g ! x



