{-# LANGUAGE LambdaCase #-}

module Data.Cell (
    Cell(..),
) where


data Cell a
    = Empty
    | Occupied a
    deriving (Show, Eq, Ord)


instance Functor Cell where
    fmap f = \case
        Empty -> Empty
        Occupied x -> Occupied $ f x



