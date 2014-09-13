module UI.Console (
) where


import Control.Monad
import Control.Monad.Prompt
import Control.Monad.Trans
import Data.Cell
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Stream as Stream
import Game.Engine
import Game.Playfield
import Game.Tetromino


pieces :: [Tetromino ()]
pieces = map (mkTetromino ()) [I, J, L, O, S, T, Z]


main :: IO ()
main = do
    score <- runConsole $ playGame bags
    print score
    where
        bag = NonEmpty.fromList pieces
        bags = Stream.repeat bag


type U = ()


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


cellToChar :: Cell U -> Char
cellToChar = \case
    Empty -> '.'
    Occupied () -> 'O'


drawBoard :: Playfield U -> Console ()
drawBoard field = liftIO $ do
    putStrLn $ replicate 80 '-'
    forM_ rows $ \row -> putStrLn $ map cellToChar row
    _ <- getChar
    return ()
    where
        rows = map (`getRow` field) [0 .. 22 - 1]
















