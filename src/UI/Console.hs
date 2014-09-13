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
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Stream as Stream
import Game.Engine
import Game.Playfield
import Game.Tetromino
import System.Console.ANSI
import System.Random
import System.Random.Shuffle


shuffleG :: RandomGen g => [a] -> g -> ([a], g)
shuffleG xs g = let
    (ns, g') = rseq (length xs) g
    in (shuffle xs ns, g')
    where
        rseq :: RandomGen g => Int -> g -> ([Int], g)
        rseq n g = let
            xs_gs = rseq' (n - 1) g
            xs = init $ map fst xs_gs
            g' = last $ map snd xs_gs
            in (xs, g')
            where
                rseq' :: RandomGen g => Int -> g -> [(Int, g)]
                rseq' 0 g = [(-1, g)]
                rseq' i g = (j, g') : rseq' (i - 1) g'
                    where
                        (j, g') = randomR (0, i) g


shuffleAll :: RandomGen g => g -> [[a]] -> [[a]]
shuffleAll _ [] = []
shuffleAll g (xs:xss) = let
    (xs', g') = shuffleG xs g
    in xs' : shuffleAll g' xss


data Block = Block {
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
            block = Block { _color = color }
            in mkTetromino block


main :: IO ()
main = do
    gen <- getStdGen
    let bags = shuffleAll gen pss
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
        SignalEvent e -> case e of
            BoardChanged field -> drawBoard field
        GetAction -> getAction


getAction :: Console Action
getAction = liftIO $ do
    return DoNothing


type ColoredChar = (Char, Color)


cellToChar :: Cell U -> ColoredChar
cellToChar = \case
    Empty -> ('.', White)
    Occupied block -> let
        c = chr 9608
        color = _color block
        in (c, color)


drawBoard :: Playfield U -> Console ()
drawBoard field = liftIO $ do
    clearScreen
    setCursorPosition 0 0
    putStrLn $ replicate (w + 2) borderChar
    forM_ rows $ \row -> do
        putChar borderChar
        forM_ row $ \cell -> let
            (c, color) = cellToChar cell
            in do
                setSGR [SetColor Foreground Vivid color]
                putChar c
        putChar borderChar
        cursorDownLine 1
    putStrLn $ replicate (w + 2) borderChar
    _ <- getChar
    return ()
    where
        borderChar = chr 9618
        dim = (10, 22)
        (w, h) = dim
        rows = map (`getRow` field) [0 .. h - 1]
















