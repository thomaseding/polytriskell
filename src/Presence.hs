module Presence (
    Presence(..),
) where


data Presence
    = NotPresent
    | Present
    deriving (Show, Eq, Ord)



