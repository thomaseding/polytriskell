module Piece (
    Piece,
) where


import Grid
import New
import Piece.Factory
import qualified Piece.I
import qualified Piece.J
import qualified Piece.L
import qualified Piece.O
import qualified Piece.S
import qualified Piece.T
import qualified Piece.Z
import Presence
import Rotate


data Piece
    = I Piece.I.I
    | J Piece.J.J
    | L Piece.L.L
    | O Piece.O.O
    | S Piece.S.S
    | T Piece.T.T
    | Z Piece.Z.Z


instance Show Piece where
    show = let
        showGrid = unlines . map (map toChar) . toList
        toChar p = case p of
            Present -> 'O'
            NotPresent -> '.'
        in showGrid . grid


instance Rotate Piece where
    rotate dir p = case p of
        I x -> I $ rotate dir x
        J x -> J $ rotate dir x
        L x -> L $ rotate dir x
        O x -> O $ rotate dir x
        S x -> S $ rotate dir x
        T x -> T $ rotate dir x
        Z x -> Z $ rotate dir x


instance New [Piece] where
    new = [
        I (new :: Piece.I.I),
        J (new :: Piece.J.J),
        L (new :: Piece.L.L),
        O (new :: Piece.O.O),
        S (new :: Piece.S.S),
        T (new :: Piece.T.T),
        Z (new :: Piece.Z.Z) ]


grid :: Piece -> Grid Presence
grid p = case p of
    I x -> Piece.I.grid x
    J x -> Piece.J.grid x
    L x -> Piece.L.grid x
    O x -> Piece.O.grid x
    S x -> Piece.S.grid x
    T x -> Piece.T.grid x
    Z x -> Piece.Z.grid x




















