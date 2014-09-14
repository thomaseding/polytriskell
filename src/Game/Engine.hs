{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Game.Engine (
    GamePrompt(..),
    LockAction,
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


type LockAction u = u -> u


data GamePrompt :: * -> * -> * where
    GetAction :: GamePrompt u Action
    BoardChanged :: Playfield u -> GamePrompt u ()
    PieceLocked :: GamePrompt u (LockAction u)


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
type GameContext p u m = (GameMonad u m, Piece p u, Functor p)


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
initGame = nextPiece


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
    prompt $ BoardChanged field


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
        startIdx = ((gw - pw) `div` 2, 0)
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
    Move rhythm moveDir -> do
        _ <- tryMove rhythm moveDir
        return ()


quitGame :: (GameContext p u m) => GameEngine p u m ()
quitGame = gameOver


gameOver :: (GameContext p u m) => GameEngine p u m ()
gameOver = modify $ \st -> st { _gameOver = True }


tickGravity :: (GameContext p u m) => GameEngine p u m ()
tickGravity = tryMove Init Down >>= \case
    True -> return ()
    False -> lockPiece


lockPiece :: (GameContext p u m) => GameEngine p u m ()
lockPiece = do
    f <- prompt PieceLocked
    modify $ \st -> st { _piece = fmap f $ _piece st }
    tryMoveM Init Nothing
    nextPiece


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


moveIndex :: Maybe MoveDir -> Index -> Index
moveIndex mDir (x, y) = case mDir of
    Nothing -> (x, y)
    Just dir -> case dir of
        Left -> (x - 1, y)
        Right -> (x + 1, y)
        Down -> (x, y + 1)


tryMove :: (GameContext p u m) => Rhythm -> MoveDir -> GameEngine p u m Bool
tryMove rhythm = tryMoveM rhythm . Just


tryMoveM :: (GameContext p u m) => Rhythm -> Maybe MoveDir -> GameEngine p u m Bool
tryMoveM _ mDir = do
    p <- gets _piece
    idx <- gets _pieceIndex
    let idx' = moveIndex mDir idx
    field <- gets $ removePiece idx p . _field
    case addPiece idx' p field of
        Nothing -> return False
        Just field' -> do
            modify $ \st -> st {
                _pieceIndex = idx',
                _field = field' }
            return True


















