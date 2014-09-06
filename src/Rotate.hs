module Rotate (
    RotateDir(..),
    Rotate(..),
) where


data RotateDir
    = Clockwise
    | CounterClockwise
    deriving (Show, Eq, Ord)


class Rotate a where
    rotate :: RotateDir -> a -> a





