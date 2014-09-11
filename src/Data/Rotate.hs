module Data.Rotate (
    RotateDir(..),
    Rotatable(..),
) where


data RotateDir
    = Clockwise
    | CounterClockwise
    deriving (Show, Eq, Ord)


class Rotatable a where
    rotate :: RotateDir -> a -> a





