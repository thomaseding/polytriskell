module Game.Level (
    Level(Level),
    unLevel,
) where


newtype Level = Level { unLevel :: Int }
    deriving (Show, Eq, Ord, Num)










