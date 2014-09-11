module Engine (
) where


import Control.Monad.Prompt
import Control.Monad.State.Lazy
import Data.Stream (Stream)
import qualified Data.Stream as Stream
import Game.Playfield
import Game.Tetromino


data GamePrompt :: * -> * where
    GetCommand :: GamePrompt Command


data Command
    = RotateClockwise
    | RotateCounterClockwise
    | MoveLeft
    | MoveRight


newtype Score = Score { unScore :: Int }
    deriving (Show, Eq, Ord, Num)


type TetrominoBag a = [Tetromino a]
type TetrominoBags a = Stream (TetrominoBag a)


data GameState a = GameState {
    _field :: Playfield a,
    _currPiece :: Maybe (Tetromino a),
    _currBag :: TetrominoBag a,
    _futureBags :: TetrominoBags a,
    _score :: Score
}


newtype GameEngine u m a = GameEngine { unGameEngine :: StateT (GameState u) m a }
    deriving (Monad, MonadState (GameState u))


type GameMonad a = MonadPrompt GamePrompt a


playGame :: (GameMonad m) => TetrominoBags a -> m Score
playGame bags = liftM _score $ flip execStateT st $ unGameEngine tickGame
    where
        st = GameState {
            _field = newPlayfield,
            _currPiece = Nothing,
            _currBag = [],
            _futureBags = bags,
            _score = 0 }


newPlayfield :: Playfield a
newPlayfield = mkPlayfield dim
    where
        dim = (10, 22)


tickGame :: (GameMonad m) => GameEngine u m ()
tickGame = undefined


ensurePiece :: (GameMonad m) => GameEngine u m ()
ensurePiece = gets _currPiece >>= \case
    Just _ -> return ()
    Nothing -> do
        bag <- gets _currBag
        case bag of
            [] -> do
                bags <- gets _futureBags
                modify $ \st -> st {
                    _currBag = Stream.head bags,
                    _futureBags = Stream.tail bags }
                ensurePiece
            piece : rest -> do
                modify $ \st -> st {
                    _currPiece = Just piece,
                    _currBag = rest }




















