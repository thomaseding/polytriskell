{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Game.Tetromino (
    TetrominoKind(..),
    Tetromino,
    mkTetromino,
    getKind,
) where


import Data.Cell (Cell(..))
import Data.Grid (Grid, Index)
import qualified Data.Grid as Grid
import Data.List (transpose)
import Data.Rotate (Rotatable(..), RotateDir(..))
import qualified Data.Rotate as Rotate
import Data.Stream (Stream)
import qualified Data.Stream as Stream
import Game.Piece (Piece(..))


data TetrominoKind = I | J | L | O | S | T | Z
    deriving (Show, Eq, Ord)


type CellGrid a = Grid (Cell a)
type Rotations a = Stream (CellGrid a)


data Tetromino a = Tetromino {
    _kind :: TetrominoKind,
    _rotations :: Rotations a
}


instance Functor Tetromino where
    fmap f (Tetromino k gs) = Tetromino k $ fmap (fmap g) gs
        where
            g = fmap f


instance Rotatable (Tetromino a) where
    rotate dir = \case
        Tetromino k gs -> Tetromino k $ case dir of
            Clockwise -> Stream.tail gs
            CounterClockwise -> Stream.drop 3 gs


instance Piece Tetromino where
    getGrid = Stream.head . _rotations
    rotate = Rotate.rotate
    overlayBy2 = overlayBy2'


overlayBy2' :: (a -> b -> Cell b) -> Index -> CellGrid a -> Tetromino b -> Tetromino b
overlayBy2' f offset grid piece = Tetromino kind rots'
    where
        kind = _kind piece
        rots = _rotations piece
        rots' = Stream.map overlay rots
        overlay = Grid.overlayBy2 g offset grid
        --
        g (Occupied x) (Occupied y) = f x y
        g _ cy = cy


mkTetromino :: a -> TetrominoKind -> Tetromino a
mkTetromino x = fmap (const x) . tetromino


getKind :: Tetromino a -> TetrominoKind
getKind (Tetromino k _) = k


tetromino :: TetrominoKind -> Tetromino ()
tetromino = \case
    I -> tetrominoI
    J -> tetrominoJ
    L -> tetrominoL
    O -> tetrominoO
    S -> tetrominoS
    T -> tetrominoT
    Z -> tetrominoZ


toGridSpec :: [[Char]] -> [[Cell ()]]
toGridSpec = enforceSquare . map (map fromChar)
    where
        isSquare xss = all (== length xss) $ map length xss
        enforceSquare xss = case isSquare xss of
            True -> xss
            False -> error "Tetromino specification must be square."
        fromChar c = case c of
            '.' -> Empty
            'O' -> Occupied ()
            _ -> error "Illegal presence specification."


mkTetromino' :: TetrominoKind -> [[Char]] -> Tetromino ()
mkTetromino' k = Tetromino k . genRotations . toGridSpec
    where
        rotateCW = map reverse . transpose
        genRotations = Stream.cycle . map Grid.fromLists . take 4 . iterate rotateCW


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







