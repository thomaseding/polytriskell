module Game.Tetromino (
    TetrominoKind(..),
    Tetromino,
    mkTetromino,
    kind,
    grid,
    metadata,
) where


import Data.Grid (Grid, fromLists)
import Data.List (transpose)
import Data.Presence (Presence(..))
import Data.Rotate (Rotate(..), RotateDir(..))
import Data.Stream (Stream)
import qualified Data.Stream as Stream


data TetrominoKind = I | J | L | O | S | T | Z
    deriving (Show, Eq, Ord)


type Rotations = Stream (Grid Presence)


data Tetromino a = Tetromino a TetrominoKind Rotations


instance Functor Tetromino where
    fmap f = \case
        Tetromino x k gs -> Tetromino (f x) k gs


instance Rotate (Tetromino a) where
    rotate dir = \case
        Tetromino x k gs -> Tetromino x k $ case dir of
            Clockwise -> Stream.tail gs
            CounterClockwise -> Stream.drop 3 gs


mkTetromino :: a -> TetrominoKind -> Tetromino a
mkTetromino x = fmap (const x) . tetromino


metadata :: Tetromino a -> a
metadata = \case
    Tetromino x _ _ -> x


kind :: Tetromino a -> TetrominoKind
kind = \case
    Tetromino _ k _ -> k


grid :: Tetromino a -> Grid Presence
grid = \case
    Tetromino _ _ gs -> Stream.head gs


tetromino :: TetrominoKind -> Tetromino ()
tetromino = \case
    I -> tetrominoI
    J -> tetrominoJ
    L -> tetrominoL
    O -> tetrominoO
    S -> tetrominoS
    T -> tetrominoT
    Z -> tetrominoZ


toGridSpec :: [[Char]] -> [[Presence]]
toGridSpec = enforceSquare . map (map fromChar)
    where
        isSquare xss = all (== length xss) $ map length xss
        enforceSquare xss = case isSquare xss of
            True -> xss
            False -> error "Tetromino specification must be square."
        fromChar c = case c of
            'O' -> Present
            '.' -> NotPresent
            _ -> error "Illegal presence specification."


mkTetromino' :: TetrominoKind -> [[Char]] -> Tetromino ()
mkTetromino' k = Tetromino () k . genRotations . toGridSpec
    where
        rotateCW = map reverse . transpose
        genRotations = Stream.cycle . map fromLists . take 4 . iterate rotateCW


tetrominoI :: Tetromino ()
tetrominoI = mkTetromino' I [
    "....",
    "OOOO",
    "....",
    "...." ]


tetrominoJ :: Tetromino ()
tetrominoJ = mkTetromino' J [
    "O..",
    "OOO",
    "..." ]


tetrominoL :: Tetromino ()
tetrominoL = mkTetromino' L [
    "..O",
    "OOO",
    "..." ]


tetrominoO :: Tetromino ()
tetrominoO = mkTetromino' O [
    "OO",
    "OO" ]


tetrominoS :: Tetromino ()
tetrominoS = mkTetromino' S [
    ".OO",
    "OO.",
    "..." ]


tetrominoT :: Tetromino ()
tetrominoT = mkTetromino' T [
    ".O.",
    "OOO",
    "..." ]


tetrominoZ :: Tetromino ()
tetrominoZ = mkTetromino' Z [
    "OO.",
    ".OO",
    "..." ]







