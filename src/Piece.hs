module Piece (
    PieceKind(..),
    Piece,
    mkPiece,
    grid,
) where


import Data.List (transpose)
import Data.Stream (Stream)
import qualified Data.Stream as Stream
import Grid (Grid, fromList)
import Presence (Presence(..))
import Rotate (Rotate(..), RotateDir(..))


data PieceKind = I | J | L | O | S | T | Z
    deriving (Show, Eq, Ord)


type Rotations = Stream (Grid Presence)


data Piece a = Piece a PieceKind Rotations


instance Rotate (Piece a) where
    rotate dir p = case p of
        Piece x kind grids -> Piece x kind $ case dir of
            Clockwise -> Stream.tail grids
            CounterClockwise -> Stream.drop 3 grids


mkPiece :: PieceKind -> a -> Piece a
mkPiece k x = Piece x k $ genRotations $ gridSpec k
    where
        rotateCW = map reverse . transpose
        genRotations = Stream.cycle . map fromList . take 4 . iterate rotateCW


grid :: Piece a -> Grid Presence
grid = \case
    Piece _ _ gs -> Stream.head gs


gridSpec :: PieceKind -> [[Presence]]
gridSpec = \case
    I -> gridSpecI
    J -> gridSpecJ
    L -> gridSpecL
    O -> gridSpecO
    S -> gridSpecS
    T -> gridSpecT
    Z -> gridSpecZ


toGridSpec :: [[Char]] -> [[Presence]]
toGridSpec = enforceSquare . map (map fromChar)
    where
        isSquare xss = all (== length xss) $ map length xss
        enforceSquare xss = case isSquare xss of
            True -> xss
            False -> error "Piece specification must be square."
        fromChar c = case c of
            'O' -> Present
            '.' -> NotPresent
            _ -> error "Illegal presence specification."


gridSpecI :: [[Presence]]
gridSpecI = toGridSpec [
    "....",
    "OOOO",
    "....",
    "...." ]


gridSpecJ :: [[Presence]]
gridSpecJ = toGridSpec [
    "O..",
    "OOO",
    "..." ]


gridSpecL :: [[Presence]]
gridSpecL = toGridSpec [
    "..O",
    "OOO",
    "..." ]


gridSpecO :: [[Presence]]
gridSpecO = toGridSpec [
    "OO",
    "OO" ]


gridSpecS :: [[Presence]]
gridSpecS = toGridSpec [
    ".OO",
    "OO.",
    "..." ]


gridSpecT :: [[Presence]]
gridSpecT = toGridSpec [
    ".O.",
    "OOO",
    "..." ]


gridSpecZ :: [[Presence]]
gridSpecZ = toGridSpec [
    "OO.",
    ".OO",
    "..." ]







