module Data.Grid (
    Width,
    Height,
    XLoc,
    YLoc,
    Grid,
    mkGrid,
    fromList,
    toList,
    Data.Grid.zipWith,
    dimensions,
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


instance Functor Grid where
    fmap f (Grid xss) = Grid $ map (map f) xss


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


zipWith :: (a -> b -> c) -> Grid a -> Grid b -> Grid c
zipWith f g1 g2 = case dimensions g1 == dimensions g2 of
    False -> error "Data.Grid.zipWith defined only for same sized grids."
    True -> let
        xss1 = unGrid g1
        xss2 = unGrid g2
        yss = Prelude.zipWith (Prelude.zipWith f) xss1 xss2
        in Grid yss


dimensions :: Grid a -> (Width, Height)
dimensions (Grid xss) = (length $ head xss, length xss)


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



