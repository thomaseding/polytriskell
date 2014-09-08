module Piece (
    Piece,
) where


import Data.List (transpose)
import Grid (Grid, fromList, toList)
import New (New(..))
import Presence (Presence(..))
import Rotate (Rotate(..), RotateDir(..))


data ShapeKind = I | J | L | O | S | T | Z
    deriving (Show, Eq, Ord)


data Piece = Piece ShapeKind [Grid Presence]


instance Show Piece where
    show = let
        showGrid = unlines . map (map toChar) . toList
        toChar p = case p of
            Present -> 'O'
            NotPresent -> '.'
        in showGrid . grid


instance Rotate Piece where
    rotate dir p = case p of
        Piece kind grids -> Piece kind $ case dir of
            Clockwise -> drop 1 grids
            CounterClockwise -> drop 3 grids


instance New [Piece] where
    new = map (\(strRep, kind) -> Piece kind $ mkRotations strRep) prototypes


mkRotations :: [String] -> [Grid Presence]
mkRotations = cycle . map fromList . take 4 . iterate rotateCW . map (map fromChar)


rotateCW :: [[a]] -> [[a]]
rotateCW = map reverse . transpose


grid :: Piece -> Grid Presence
grid p = case p of
    Piece _ (g:gs) -> g


prototypes :: [([String], ShapeKind)]
prototypes = [
    (prototypeI, I),
    (prototypeJ, J),
    (prototypeL, L),
    (prototypeO, O),
    (prototypeS, S),
    (prototypeT, T),
    (prototypeZ, Z) ]


fromChar :: Char -> Presence
fromChar c = case c of
    'O' -> Present
    '.' -> NotPresent
    _ -> error "Illegal presence specification."


prototypeI :: [String]
prototypeI = [
    "....",
    "OOOO",
    "....",
    "...." ]


prototypeJ :: [String]
prototypeJ = [
    "O..",
    "OOO",
    "..." ]


prototypeL :: [String]
prototypeL = [
    "..O",
    "OOO",
    "..." ]


prototypeO :: [String]
prototypeO = [
    "OO",
    "OO" ]


prototypeS :: [String]
prototypeS = [
    ".OO",
    "OO.",
    "..." ]


prototypeT :: [String]
prototypeT = [
    ".O.",
    "OOO",
    "..." ]


prototypeZ :: [String]
prototypeZ = [
    "OO.",
    ".OO",
    "..." ]







