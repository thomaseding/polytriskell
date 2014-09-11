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
    = DoNothing
    | RotateClockwise
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
    _futurePieces :: Stream (Tetromino a),
    _score :: Score
}


newtype GameEngine u m a = GameEngine { unGameEngine :: StateT (GameState u) m a }
    deriving (Monad, MonadState (GameState u), MonadTrans)


type GameMonad a = MonadPrompt GamePrompt a


instance (GameMonad m) => MonadPrompt GamePrompt (GameEngine u m) where
    prompt = lift . prompt


newPlayfield :: Playfield a
newPlayfield = mkPlayfield dim
    where
        dim = (10, 22)


playGame :: (GameMonad m) => TetrominoBags a -> m Score
playGame bags = liftM _score $ flip execStateT st $ unGameEngine runGame
    where
        st = GameState {
            _field = newPlayfield,
            _currPiece = Nothing,
            _futurePieces = Stream.fromList $ concat $ Stream.toList bags,
            _score = 0 }


runGame :: (GameMonad m) => GameEngine u m ()
runGame = isGameOver >>= \case
    True -> return ()
    False -> tickGame


tickGame :: (GameMonad m) => GameEngine u m ()
tickGame = do
    ensurePiece
    cmd <- prompt GetCommand
    performCommand cmd


isGameOver :: (GameMonad m) => GameEngine u m Bool
isGameOver = undefined


ensurePiece :: (GameMonad m) => GameEngine u m ()
ensurePiece = gets _currPiece >>= \case
    Just _ -> return ()
    Nothing -> do
        pieces <- gets _futurePieces
        modify $ \st -> st {
            _currPiece = Just $ Stream.head pieces,
            _futurePieces = Stream.tail pieces }


performCommand :: (GameMonad m) => Command -> GameEngine u m ()
performCommand = undefined


















