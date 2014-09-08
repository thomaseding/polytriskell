module Piece.Factory (
    mkRotations,
) where


import Data.List
import Grid
import Presence


fromChar :: Char -> Presence
fromChar c = case c of
    'O' -> Present
    '.' -> NotPresent
    _ -> error "Illegal presence specification."


mkRotations :: [String] -> [Grid Presence]
mkRotations = cycle . map fromList . take 4 . iterate rotateCW . map (map fromChar)


rotateCW :: [[a]] -> [[a]]
rotateCW = map reverse . transpose




