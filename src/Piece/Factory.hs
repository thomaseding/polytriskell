module Piece.Factory (
    mkPiece,
) where


import Grid
import Presence


fromChar :: Char -> Presence
fromChar c = case c of
    'O' -> Present
    '.' -> NotPresent
    _ -> error "Illegal presence specification."


mkPiece :: [String] -> Grid Presence
mkPiece = fromList . map (map fromChar)






