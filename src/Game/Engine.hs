module Game.Engine (
    GamePrompt(..),
    Event(..),
    MoveDir(..),
    Rhythm(..),
    Action(..),
    GameMonad,
    Score,
    playGame,
) where


import Control.Monad.Prompt
import Control.Monad.State.Lazy
import Data.Grid (Index, Dimensions)
import qualified Data.Grid as Grid
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NonEmpty
import Data.Rotate
import Data.Stream (Stream)
import qualified Data.Stream as Stream
import Game.Piece
import Game.Playfield
import Prelude hiding (Left, Right)


data GamePrompt :: * -> * -> * where
    GetAction :: GamePrompt a Action
    SignalEvent :: Event u -> GamePrompt u ()


data Event u
    = BoardChanged (Playfield u)


data MoveDir = Left | Right | Down
    deriving (Show, Eq, Ord)


data Rhythm = Init | Continue
    deriving (Show, Eq, Ord)


data Action
    = DoNothing
    | QuitGame
    | Rotate RotateDir
    | Move Rhythm MoveDir
    deriving (Show, Eq, Ord)


newtype Score = Score { unScore :: Int }
    deriving (Show, Eq, Ord, Num)


data GameState p u = GameState {
    _field :: Playfield u,
    _piece :: p u,
    _pieceIndex :: Index,
    _futurePieces :: Stream (p u),
    _score :: Score,
    _gameOver :: Bool
}


newtype GameEngine p u m a = GameEngine {
    unGameEngine :: StateT (GameState p u) m a
} deriving (Monad, MonadState (GameState p u), MonadTrans)


type GameMonad u a = MonadPrompt (GamePrompt u) a
type GameContext p u m = (GameMonad u m, Piece p u)


instance (GameMonad u m) => MonadPrompt (GamePrompt u) (GameEngine p u m) where
    prompt = lift . prompt


newPlayfield :: Playfield a
newPlayfield = mkPlayfield gameDim


gameDim :: Dimensions
gameDim = (10, 22)


playGame :: (GameContext p a m) => Stream (NonEmpty (p a)) -> m Score
playGame bags = liftM _score $ flip execStateT st $ unGameEngine runGame
    where
        pieces = Stream.fromList $ concat $ map NonEmpty.toList $ Stream.toList bags
        st = GameState {
            _field = newPlayfield,
            _piece = error "_piece",
            _pieceIndex = error "_pieceIndex",
            _futurePieces = pieces,
            _score = 0,
            _gameOver = False }


runGame :: (GameContext p u m) => GameEngine p u m ()
runGame = do
    initGame
    gameLoop


initGame :: (GameContext p u m) => GameEngine p u m ()
initGame = do
    nextPiece


gameLoop :: (GameContext p u m) => GameEngine p u m ()
gameLoop = isGameOver >>= \case
    True -> return ()
    False -> do
        tickGame
        gameLoop


tickGame :: (GameContext p u m) => GameEngine p u m ()
tickGame = do
    action <- prompt GetAction
    performAction action
    tickGravity
    field <- gets _field
    prompt $ SignalEvent $ BoardChanged field


isGameOver :: (GameContext p u m) => GameEngine p u m Bool
isGameOver = gets _gameOver


nextPiece :: (GameContext p u m) => GameEngine p u m ()
nextPiece = do
    pieces <- gets _futurePieces
    modify $ \st -> st {
        _piece = Stream.head pieces,
        _futurePieces = Stream.tail pieces }
    field <- gets _field
    p <- gets _piece
    let grid = getGrid p
        (pw, ph) = Grid.dimensions grid
        (gw, gh) = gameDim
        startIdx = ((gw - pw) `div` 2, 8)
    case addPiece startIdx p field of
        Nothing -> gameOver
        Just field' -> modify $ \st -> st {
            _pieceIndex = startIdx,
            _field = field' }



performAction :: (GameContext p u m) => Action -> GameEngine p u m ()
performAction = \case
    DoNothing -> return ()
    QuitGame -> quitGame
    Rotate rotateDir -> tryRotate rotateDir
    Move rhythm moveDir -> tryMove rhythm moveDir


quitGame :: (GameContext p u m) => GameEngine p u m ()
quitGame = gameOver


gameOver :: (GameContext p u m) => GameEngine p u m ()
gameOver = modify $ \st -> st { _gameOver = True }


tickGravity :: (GameContext p u m) => GameEngine p u m ()
tickGravity = tryMove Init Down


tryRotate :: (GameContext p u m) => RotateDir -> GameEngine p u m ()
tryRotate dir = do
    p <- gets _piece
    let p' = rotate dir p
    idx <- gets _pieceIndex
    field <- gets $ removePiece idx p . _field
    case addPiece idx p' field of
        Nothing -> return ()
        Just field' -> modify $ \st -> st {
            _piece = p',
            _field = field' }


moveIndex :: MoveDir -> Index -> Index
moveIndex dir (x, y) = case dir of
    Left -> (x - 1, y)
    Right -> (x + 1, y)
    Down -> (x, y + 1)


tryMove :: (GameContext p u m) => Rhythm -> MoveDir -> GameEngine p u m ()
tryMove _ dir = do
    p <- gets _piece
    idx <- gets _pieceIndex
    let idx' = moveIndex dir idx
    field <- gets $ removePiece idx p . _field
    case addPiece idx' p field of
        Nothing -> return ()
        Just field' -> modify $ \st -> st {
            _pieceIndex = idx',
            _field = field' }


















