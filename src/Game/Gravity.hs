module Game.Gravity (
    Gravity(Gravity),
    unGravity,
) where


import Data.Ratio


newtype Gravity = Gravity { unGravity :: Ratio Int }
    deriving (Show, Eq, Ord, Num, Fractional)








