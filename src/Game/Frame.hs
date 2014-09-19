module Game.Frame (
    Frame(Frame),
    unFrame,
) where


newtype Frame = Frame { unFrame :: Int }
    deriving (Show, Eq, Ord, Num)










