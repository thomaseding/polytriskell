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
    Level,
    playGame,
) where


import Control.Monad.Prompt
import Control.Monad.State.Lazy
import Data.Cell
import Data.Foldable (foldr)
import Data.Grid (Index, Dimensions)
import qualified Data.Grid as Grid
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NonEmpty
import Data.Maybe (mapMaybe)
import Data.Rotate
import Data.Stream (Stream)
import qualified Data.Stream as Stream
import Game.Piece
import Game.Playfield (Playfield)
import qualified Game.Playfield as Field
import Prelude hiding (Left, Right, foldr)


type LockAction u = u -> u
type RowCount = Int
type TotalRowCount = RowCount


data GamePrompt :: * -> * -> * where
    GetAction :: GamePrompt u Action
    ScoreChanged :: Score -> GamePrompt u ()
    LevelChanged :: Level -> GamePrompt u ()
    BoardChanged :: Playfield u -> GamePrompt u ()
    RowsCleared :: NonEmpty Int -> TotalRowCount -> GamePrompt u ()
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


newtype Level = Level { unLevel :: Int }
    deriving (Eq, Ord, Num)


instance Show Level where
    show = show . unLevel


newtype Score = Score { unScore :: Int }
    deriving (Eq, Ord, Num)


instance Show Score where
    show = show . unScore


data GameState p u = GameState {
    _field :: Playfield u,
    _piece :: p u,
    _pieceIndex :: Index,
    _futurePieces :: Stream (p u),
    _rowsCleared :: TotalRowCount,
    _level :: Level,
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
newPlayfield = Field.mkPlayfield gameDim


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
            _level = 1,
            _rowsCleared = 0,
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
    case Field.addPiece startIdx p field of
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
    tryMoveBy id
    tryClearRows
    nextPiece


tryRotate :: (GameContext p u m) => RotateDir -> GameEngine p u m ()
tryRotate dir = do
    p <- gets _piece
    let p' = rotate dir p
    idx <- gets _pieceIndex
    field <- gets $ Field.removePiece idx p . _field
    case Field.addPiece idx p' field of
        Nothing -> return ()
        Just field' -> modify $ \st -> st {
            _piece = p',
            _field = field' }


moveIndex :: MoveDir -> Index -> Index
moveIndex dir (x, y) = case dir of
    Left -> (x - 1, y)
    Right -> (x + 1, y)
    Down -> (x, y + 1)


tryMove :: (GameContext p u m) => Rhythm -> MoveDir -> GameEngine p u m Bool
tryMove _ = tryMoveBy . moveIndex


tryMoveBy :: (GameContext p u m) => (Index -> Index) -> GameEngine p u m Bool
tryMoveBy fIndex = do
    p <- gets _piece
    idx <- gets _pieceIndex
    let idx' = fIndex idx
    field <- gets $ Field.removePiece idx p . _field
    case Field.addPiece idx' p field of
        Nothing -> return False
        Just field' -> do
            modify $ \st -> st {
                _pieceIndex = idx',
                _field = field' }
            return True


getRows :: (GameContext p u m) => GameEngine p u m [[Cell u]]
getRows = do
    field <- gets _field
    let (_, h) = gameDim
        idxs = [0 .. h - 1]
        rows = map (`Field.getRow` field) idxs
    return rows


isFull :: [Cell a] -> Bool
isFull = and . map f
    where
        f = \case
            Empty -> False
            _ -> True


tryClearRows :: (GameContext p u m) => GameEngine p u m ()
tryClearRows = do
    rows <- getRows
    let idxs = flip mapMaybe (zip rows [0..]) $ \(row, idx) -> case isFull row of
            False -> Nothing
            True -> Just idx
    case NonEmpty.nonEmpty idxs of
        Nothing -> return ()
        Just idxs' -> clearRows idxs'


clearRows :: (GameContext p u m) => NonEmpty Int -> GameEngine p u m ()
clearRows idxs = do
    mapM clearRow $ NonEmpty.toList idxs
    updateScore $ NonEmpty.length idxs
    gets _rowsCleared >>= prompt . RowsCleared idxs
    gets _score >>= prompt . ScoreChanged
    gets _field >>= prompt . BoardChanged


clearRow :: (GameContext p u m) => Int -> GameEngine p u m ()
clearRow idx = do
    field <- gets $ Field.clearRow idx . _field
    rowsCleared <- gets _rowsCleared
    let field' = Field.dropRowsAbove idx field
        rowsCleared' = rowsCleared + 1
        level = Level $ (rowsCleared' `div` 10) + 1
    modify $ \st -> st {
        _level = level,
        _rowsCleared = rowsCleared',
        _field = field' }


updateScore :: (GameContext p u m) => RowCount -> GameEngine p u m ()
updateScore numRowsCleared = do
    level <- gets _level
    score <- gets _score
    let points = calculatePoints level numRowsCleared
        score' = score + points
    modify $ \st -> st { _score = score' }
    prompt $ ScoreChanged score'


calculatePoints :: Level -> RowCount -> Score
calculatePoints level = Score . (unLevel level *) . \case
    1 -> 100
    2 -> 300
    3 -> 500
    4 -> 800
    n -> error $ "Don't know how to score " ++ show n ++ " rows cleared."












