module Game.Score (
    Score(Score),
    unScore,
) where


newtype Score = Score { unScore :: Int }
    deriving (Show, Eq, Ord, Num)










