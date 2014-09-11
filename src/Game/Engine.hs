module Engine (
) where


import Control.Monad.Prompt
import Control.Monad.State.Lazy
import Data.Rotate
import Data.Stream (Stream)
import qualified Data.Stream as Stream
import Game.Piece
import Game.Playfield
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


type Bag a = [a]


data GameState p u = GameState {
    _field :: Playfield u,
    _currPiece :: Maybe (p u),
    _futurePieces :: Stream (p u),
    _score :: Score
}


newtype GameEngine p u m a = GameEngine {
    unGameEngine :: StateT (GameState p u) m a
} deriving (Monad, MonadState (GameState p u), MonadTrans)


type GameMonad a = MonadPrompt GamePrompt a


instance (GameMonad m) => MonadPrompt GamePrompt (GameEngine p u m) where
    prompt = lift . prompt


newPlayfield :: Playfield a
newPlayfield = mkPlayfield dim
    where
        dim = (10, 22)


playGame :: (GameMonad m, Piece p a) => Stream (Bag (p a)) -> m Score
playGame bags = liftM _score $ flip execStateT st $ unGameEngine runGame
    where
        st = GameState {
            _field = newPlayfield,
            _currPiece = Nothing,
            _futurePieces = Stream.fromList $ concat $ Stream.toList bags,
            _score = 0 }


runGame :: (GameMonad m) => GameEngine p u m ()
runGame = isGameOver >>= \case
    True -> return ()
    False -> tickGame


tickGame :: (GameMonad m) => GameEngine p u m ()
tickGame = do
    ensurePiece
    action <- prompt GetAction
    performAction action
    tickGravity


isGameOver :: (GameMonad m) => GameEngine p u m Bool
isGameOver = undefined


ensurePiece :: (GameMonad m) => GameEngine p u m ()
ensurePiece = gets _currPiece >>= \case
    Just _ -> return ()
    Nothing -> do
        pieces <- gets _futurePieces
        modify $ \st -> st {
            _currPiece = Just $ Stream.head pieces,
            _futurePieces = Stream.tail pieces }


performAction :: (GameMonad m) => Action -> GameEngine p u m ()
performAction = \case
    DoNothing -> return ()
    Rotate rotateDir -> tryRotate rotateDir
    Move rhythm moveDir -> tryMove rhythm moveDir


tickGravity :: (GameMonad m) => GameEngine p u m ()
tickGravity = undefined


tryRotate :: (GameMonad m) => RotateDir -> GameEngine p u m ()
tryRotate = undefined


tryMove :: (GameMonad m) => Rhythm -> MoveDir -> GameEngine p u m ()
tryMove = undefined


















