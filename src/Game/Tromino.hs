module Game.Tromino (
    TrominoKind(..),
    Tromino,
    mkTromino,
    kind,
    grid,
    metadata,
) where


import Data.Grid (Grid, fromList)
import Data.List (transpose)
import Data.Presence (Presence(..))
import Data.Rotate (Rotate(..), RotateDir(..))
import Data.Stream (Stream)
import qualified Data.Stream as Stream


data TrominoKind = I | J | L | O | S | T | Z
    deriving (Show, Eq, Ord)


type Rotations = Stream (Grid Presence)


data Tromino a = Tromino a TrominoKind Rotations


instance Functor Tromino where
    fmap f = \case
        Tromino x k gs -> Tromino (f x) k gs


instance Rotate (Tromino a) where
    rotate dir = \case
        Tromino x k gs -> Tromino x k $ case dir of
            Clockwise -> Stream.tail gs
            CounterClockwise -> Stream.drop 3 gs


mkTromino :: a -> TrominoKind -> Tromino a
mkTromino x = fmap (const x) . tromino


metadata :: Tromino a -> a
metadata = \case
    Tromino x _ _ -> x


kind :: Tromino a -> TrominoKind
kind = \case
    Tromino _ k _ -> k


grid :: Tromino a -> Grid Presence
grid = \case
    Tromino _ _ gs -> Stream.head gs


tromino :: TrominoKind -> Tromino ()
tromino = \case
    I -> trominoI
    J -> trominoJ
    L -> trominoL
    O -> trominoO
    S -> trominoS
    T -> trominoT
    Z -> trominoZ


toGridSpec :: [[Char]] -> [[Presence]]
toGridSpec = enforceSquare . map (map fromChar)
    where
        isSquare xss = all (== length xss) $ map length xss
        enforceSquare xss = case isSquare xss of
            True -> xss
            False -> error "Tromino specification must be square."
        fromChar c = case c of
            'O' -> Present
            '.' -> NotPresent
            _ -> error "Illegal presence specification."


mkTromino' :: TrominoKind -> [[Char]] -> Tromino ()
mkTromino' k = Tromino () k . genRotations . toGridSpec
    where
        rotateCW = map reverse . transpose
        genRotations = Stream.cycle . map fromList . take 4 . iterate rotateCW


trominoI :: Tromino ()
trominoI = mkTromino' I [
    "....",
    "OOOO",
    "....",
    "...." ]


trominoJ :: Tromino ()
trominoJ = mkTromino' J [
    "O..",
    "OOO",
    "..." ]


trominoL :: Tromino ()
trominoL = mkTromino' L [
    "..O",
    "OOO",
    "..." ]


trominoO :: Tromino ()
trominoO = mkTromino' O [
    "OO",
    "OO" ]


trominoS :: Tromino ()
trominoS = mkTromino' S [
    ".OO",
    "OO.",
    "..." ]


trominoT :: Tromino ()
trominoT = mkTromino' T [
    ".O.",
    "OOO",
    "..." ]


trominoZ :: Tromino ()
trominoZ = mkTromino' Z [
    "OO.",
    ".OO",
    "..." ]







