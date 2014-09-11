module Engine (
) where


import Control.Monad.Prompt
import Control.Monad.State.Lazy
import Data.Rotate
import Data.Stream (Stream)
import qualified Data.Stream as Stream
import Game.Playfield
import Game.Tetromino
import Prelude hiding (Left, Right)


data GamePrompt :: * -> * where
    GetAction :: GamePrompt Action
    SignalEvent :: Event -> GamePrompt ()


data Event


data MoveDir = Left | Right
    deriving (Show, Eq, Ord)


data Rhythm = Init | Continue
    deriving (Show, Eq, Ord)


data Action
    = DoNothing
    | Rotate RotateDir
    | Move Rhythm MoveDir
    deriving (Show, Eq, Ord)


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
    action <- prompt GetAction
    performAction action
    tickGravity


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


performAction :: (GameMonad m) => Action -> GameEngine u m ()
performAction = \case
    DoNothing -> return ()
    Rotate rotateDir -> tryRotate rotateDir
    Move rhythm moveDir -> tryMove rhythm moveDir


tickGravity :: (GameMonad m) => GameEngine u m ()
tickGravity = undefined


tryRotate :: (GameMonad m) => RotateDir -> GameEngine u m ()
tryRotate = undefined


tryMove :: (GameMonad m) => Rhythm -> MoveDir -> GameEngine u m ()
tryMove = undefined


















