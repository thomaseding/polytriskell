{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module UI.GUI (
    main,
) where


import Control.Concurrent
import Control.Monad
import Control.Monad.Reader
import Control.Monad.Prompt
import Control.Monad.Trans
import Data.Cell
import Data.Char
import Data.List.NonEmpty (NonEmpty)
import Data.Ratio
import Data.Rotate
import Data.Stream (Stream)
import qualified Data.Stream as Stream
import Data.Word
import Game.Engine
import Game.Gravity
import Game.Level
import Game.Playfield
import Game.Score
import Game.Tetromino
import Graphics.UI.Gtk (AttrOp((:=)))
import qualified Graphics.UI.Gtk as Gtk
import Prelude hiding (Left, Right)
import System.Random
import System.Random.Shuffle
import System.Timeout


main :: IO ()
main = do
    Gtk.initGUI
    window <- Gtk.windowNew
    image <- Gtk.imageNewFromFile "/home/thomas/Downloads/pwg-sample-11_photo.jpg"
    Gtk.set window [
        Gtk.containerChild := image,
        Gtk.containerBorderWidth := 0 ]
    Gtk.onDestroy window Gtk.mainQuit
    Gtk.widgetShowAll window
    Gtk.windowFullscreen window
    --
    when True $ do
        forkIO $ do
            threadDelay 1000000 -- TODO: See if this is needed
            gen <- getStdGen
            let ps = Stream.fromList $ concat $ shuffleAll basicBags gen
                env = Env { _image = image }
            score <- runGUI env $ playGame gameConfig ps
            Gtk.widgetDestroy window
        return ()
    --
    Gtk.mainGUI
    where
        basicBags = repeat basicPieces


data Env = Env {
    _image :: Gtk.Image
}


splits :: RandomGen g => g -> [g]
splits g = let
    (g1, g2) = split g
    in g1 : splits g2


shuffleAll :: RandomGen g => [[a]] -> g -> [[a]]
shuffleAll xss = zipWith f xss . splits
    where
        f xs = shuffle' xs (length xs)


type Color = (Word8, Word8, Word8, Word8)


data Block = Block {
    _color :: Color
}


type U = Block


basicPieces :: [Tetromino U]
basicPieces = [
    mk I (255,0,0,0),
    mk J (0,255,0,0),
    mk L (0,0,255,0),
    mk O (255,255,0,0),
    mk S (0,255,255,0),
    mk T (255,0,255,0),
    mk Z (255,255,255,0)]
    where
        mk kind color = let
            block = Block { _color = color }
            in mkTetromino block kind


gameConfig :: GameConfig U
gameConfig = defaultGameConfig {
    _lockAction = lockAction,
    _lockDelay = const 0,
    _ghostify = Just $ ghostify,
    _gravityRate = const $ Gravity $ 1 % frameRate
}


ghostify :: U -> U
ghostify block = block { _color = color' }
    where
        (r,g,b,a) = _color block
        f = (`div` 2)
        color' = (f r, f g, f b, a)


newtype GUI a = GUI { unGUI :: ReaderT Env IO a }
    deriving (Functor, Monad, MonadIO, MonadReader Env)


runGUI :: Env -> GUI a -> IO a
runGUI env gui = runReaderT (unGUI gui) env


instance MonadPrompt (GamePrompt Tetromino U) GUI where
    prompt = \case
        PlayfieldChanged field -> drawBoard field
        RowsCleared rows totalCleared -> rowsCleared rows totalCleared
        GetAction -> getAction
        ScoreChanged s -> drawScore s
        LevelChanged l -> drawLevel l
        PiecePreview ps -> piecePreview ps


drawBoard :: Playfield U -> GUI ()
drawBoard field = do
    liftIO $ threadDelay microsecondsPerFrame
    image <- asks _image
    liftIO $ Gtk.postGUISync $ do
        pix <- Gtk.imageGetPixbuf image
        forM_ (zip cellss [0..]) $ \(cells, row) -> do
            forM_ (zip cells [0..]) $ \(cell, col) -> do
                drawCell pix row col cell
        Gtk.imageSetFromPixbuf image pix
    where
        (_, h) = _playfieldDim gameConfig
        cellss = map (`getRow` field) [0 .. h - 1]


drawCell :: Gtk.Pixbuf -> Int -> Int -> Cell U -> IO ()
drawCell pix row col cell = do
    tmp <- Gtk.pixbufNew Gtk.ColorspaceRgb alphaBool 8 cellDim cellDim
    Gtk.pixbufFill tmp r g b a
    Gtk.pixbufCopyArea
        tmp 0 0 cellDim cellDim
        pix (col * cellDim) (row * cellDim)
    where
        alphaBool = False
        cellDim = 20
        (r, g, b, a) = case cell of
            Empty -> (0, 0, 0, 0)
            Occupied block -> _color block


drawScore _ = return ()
drawLevel _ = return ()
piecePreview _ = return ()
drawRowsCleared _ = return ()


rowsCleared :: NonEmpty Int -> Int -> GUI ()
rowsCleared _ totalCleared = do
    drawRowsCleared totalCleared


lockAction :: LockAction U
lockAction = id


frameRate :: Int
frameRate = 20


microsecondsPerFrame :: Int
microsecondsPerFrame = 1000000 `div` frameRate


getAction :: GUI Action
getAction = do
    return $ Move Init Down








