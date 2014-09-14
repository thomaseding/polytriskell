{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module UI.Console (
) where


import Control.Monad
import Control.Monad.Prompt
import Control.Monad.Trans
import Data.Cell
import Data.Char
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NonEmpty
import Data.Rotate
import qualified Data.Stream as Stream
import Game.Engine
import Game.Playfield
import Game.Tetromino
import Prelude hiding (Left, Right)
import System.Console.ANSI
import System.IO
import System.Random
import System.Random.Shuffle
import System.Timeout


splits :: RandomGen g => g -> [g]
splits g = let
    (g1, g2) = split g
    in g1 : splits g2


shuffleAll :: RandomGen g => [[a]] -> g -> [[a]]
shuffleAll xss = zipWith f xss . splits
    where
        f xs = shuffle' xs (length xs)


data Block = Block {
    _intensity :: ColorIntensity,
    _char :: Char,
    _color :: Color
}


type U = Block


pieces :: [Tetromino U]
pieces = [
    mk Cyan I,
    mk Blue J,
    mk White L,
    mk Yellow O,
    mk Green S,
    mk Magenta T,
    mk Red Z ]
    where
        mk color = let
            block = Block {
                _intensity = Vivid,
                _char = chr 9608,
                _color = color }
            in mkTetromino block


main :: IO ()
main = do
    hSetBuffering stdin NoBuffering
    gen <- getStdGen
    let bags = shuffleAll pss gen
    let bags' = Stream.fromList $ map NonEmpty.fromList bags
    score <- runConsole $ playGame bags'
    print score
    where
        ps = pieces
        pss = repeat ps


newtype Console a = Console { runConsole :: IO a }
    deriving (Functor, Monad, MonadIO)


instance MonadPrompt (GamePrompt U) Console where
    prompt = \case
        BoardChanged field -> drawBoard field
        RowsCleared rows -> rowsCleared rows
        GetAction -> getAction
        PieceLocked -> lockAction


rowsCleared :: NonEmpty Int -> Console ()
rowsCleared _ = return ()


lockAction :: Console (LockAction U)
lockAction = return $ \block -> block {
    _intensity = Dull }


getAction :: Console Action
getAction = liftIO $ do
    timeout 1000000 getChar >>= clearChars . \case
        Just 'a' -> Move Init Left
        Just 'd' -> Move Init Right
        Just 'q' -> Rotate CounterClockwise
        Just 'e' -> Rotate Clockwise
        _ -> DoNothing
    where
        clearChars c = do
            _ <- timeout 100 $ replicateM_ 10000 getChar
            return c


type ColoredChar = (Char, Color, ColorIntensity)


cellToChar :: Cell U -> ColoredChar
cellToChar = \case
    Empty -> (' ', White, Vivid)
    Occupied block -> let
        intensity = _intensity block
        char = _char block
        color = _color block
        in (char, color, intensity)


drawBoard :: Playfield U -> Console ()
drawBoard field = liftIO $ do
    clearScreen
    setCursorPosition 0 0
    setSGR [Reset]
    putStrLn2 $ replicate (w + 2) borderChar
    forM_ rows $ \row -> do
        putChar2 borderChar
        forM_ row $ \cell -> let
            (c, color, intensity) = cellToChar cell
            in do
                setSGR [SetColor Foreground intensity color]
                putChar2 c
        setSGR [Reset]
        putChar2 borderChar
        cursorDownLine 1
    putStrLn2 $ replicate (w + 2) borderChar
    return ()
    where
        borderChar = chr 9618
        dim = (10, 22)
        (w, h) = dim
        rows = map (`getRow` field) [0 .. h - 1]
        putChar2 c = putChar c >> putChar c
        putStrLn2 s = mapM_ putChar2 s >> putChar '\n'
















