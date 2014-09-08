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


type Width = Int
type Height = Int


type XLoc = Int
type YLoc = Int


newtype Grid a = Grid { unGrid :: [[a]] }
    deriving (Eq, Ord)


instance Show a => Show (Grid a) where
    show = show . toList


toList :: Grid a -> [[a]]
toList = unGrid


mkGrid :: Width -> Height -> a -> Grid a
mkGrid w h x = Grid $ replicate h row
    where
        row = take w $ repeat x


fromList :: [[a]] -> Grid a
fromList = Grid . ensureRect
    where
        allSame xs = all (== head xs) xs
        isRect = allSame . map length
        ensureRect xss = case isRect xss of
            True -> xss
            False -> error "Grid must be constructed from NxM list of values."


putRow :: [a] -> Int -> Grid a -> Grid a
putRow row n (Grid rs) = let
    leadingRs = take n rs
    trailingRs = drop (n + 1) rs
    in Grid $ leadingRs ++ [row] ++ trailingRs


getRow :: Int -> Grid a -> [a]
getRow n = head . drop n . unGrid


replaceNth :: Int -> a -> [a] -> [a]
replaceNth _ _ [] = error "Cannot replace a value in an empty list."
replaceNth n x' (x:xs) = case n of
    0 -> x' : xs
    _ -> x : replaceNth (n - 1) x' xs


put :: a -> XLoc -> YLoc -> Grid a -> Grid a
put val x y g = let
    row = getRow y g
    row' = replaceNth x val row
    in putRow row' y g


get :: XLoc -> YLoc -> Grid a -> a
get x y g = getRow y g !! x



