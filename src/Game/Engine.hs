{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Game.Engine (
    GameConfig(..),
    defaultGameConfig,
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


import Control.Monad.Loops (whileM_)
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


branch :: (MonadState s m) => (s -> s) -> m a -> m a
branch f action = do
    snapshot <- get
    result <- action
    put $ f snapshot
    return result


subtractIndex :: Index -> Index -> Index
subtractIndex (x, y) (x', y') = (x - x', y - y')


type LockAction u = u -> u
type RowCount = Int
type TotalRowCount = RowCount


data GamePrompt :: * -> * -> * where
    GetAction :: GamePrompt u Action
    ScoreChanged :: Score -> GamePrompt u ()
    LevelChanged :: Level -> GamePrompt u ()
    PlayfieldChanged :: Playfield u -> GamePrompt u ()
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


data GameConfig u = GameConfig {
    _ghostFunc :: Maybe (u -> u),
    _playfieldDim :: Dimensions
}


data GameState p u = GameState {
    _config :: GameConfig u,
    _field :: Playfield u,
    _piece :: p u,
    _pieceIndex :: Index,
    _ghostPieceIndex :: Maybe Index,
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


defaultGameConfig :: GameConfig u
defaultGameConfig = GameConfig {
    _ghostFunc = Nothing,
    _playfieldDim = (10, 22)
}


playGame :: (GameContext p u m) => GameConfig u -> Stream (p u) -> m Score
playGame config pieces = liftM _score $ flip execStateT st $ unGameEngine runGame
    where
        st = GameState {
            _config = config,
            _field = Field.mkPlayfield $ _playfieldDim config,
            _piece = Stream.head pieces,
            _pieceIndex = (0, 0),
            _ghostPieceIndex = Nothing,
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
    prompt $ PlayfieldChanged field


isGameOver :: (GameContext p u m) => GameEngine p u m Bool
isGameOver = gets _gameOver


nextPiece :: (GameContext p u m) => GameEngine p u m ()
nextPiece = do
    removeGhostPiece
    dim <- gets $ _playfieldDim . _config
    pieces <- gets _futurePieces
    modify $ \st -> st {
        _piece = Stream.head pieces,
        _futurePieces = Stream.tail pieces }
    field <- gets _field
    p <- gets _piece
    let grid = getGrid p
        (pw, ph) = Grid.dimensions grid
        (gw, gh) = dim
        startIdx = ((gw - pw) `div` 2, 0)
    case Field.addPiece startIdx p field of
        Nothing -> gameOver
        Just field' -> do
            modify $ \st -> st {
                _pieceIndex = startIdx,
                _field = field' }
            _ <- tryMoveBy id -- to trigger ghost piece
            return ()



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
    removeGhostPiece
    p <- gets _piece
    let p' = rotate dir p
    idx <- gets _pieceIndex
    field <- gets $ Field.removePiece idx p . _field
    case Field.addPiece idx p' field of
        Nothing -> return ()
        Just field' -> modify $ \st -> st {
            _piece = p',
            _field = field' }
    addGhostPiece


moveIndex :: MoveDir -> Index -> Index
moveIndex dir (x, y) = case dir of
    Left -> (x - 1, y)
    Right -> (x + 1, y)
    Down -> (x, y + 1)


tryMove :: (GameContext p u m) => Rhythm -> MoveDir -> GameEngine p u m Bool
tryMove _ = tryMoveBy . moveIndex


tryMoveBy :: (GameContext p u m) => (Index -> Index) -> GameEngine p u m Bool
tryMoveBy fIndex = do
    removeGhostPiece
    p <- gets _piece
    idx <- gets _pieceIndex
    let idx' = fIndex idx
    field <- gets $ Field.removePiece idx p . _field
    moved <- case Field.addPiece idx' p field of
        Nothing -> return False
        Just field' -> do
            modify $ \st -> st {
                _pieceIndex = idx',
                _field = field' }
            return True
    addGhostPiece
    return moved


addGhostPiece :: (GameContext p u m) => GameEngine p u m ()
addGhostPiece = gets (_ghostFunc . _config) >>= \case
    Nothing -> return ()
    Just f -> do
        (ghost, ghostIndex) <- getCroppedGhost f
        field <- gets _field
        case Field.addPiece ghostIndex ghost field of
            Nothing -> return ()
            Just field' -> modify $ \st -> st {
                _ghostPieceIndex = Just ghostIndex,
                _field = field' }


removeGhostPiece :: (GameContext p u m) => GameEngine p u m ()
removeGhostPiece = gets (_ghostFunc . _config) >>= \case
    Nothing -> return ()
    Just f -> do
        (ghost, ghostIndex) <- getCroppedGhost f
        field <- gets _field
        let field' = Field.removePiece ghostIndex ghost field
        modify $ \st -> st {
            _ghostPieceIndex = Nothing,
            _field = field' }


getGhost :: (GameContext p u m) => (u -> u) -> GameEngine p u m (p u, Index)
getGhost f = branch id $ do
    modify $ \st -> let
        config = _config st
        config' = config { _ghostFunc = Nothing }
        in st {
            _config = config',
            _piece = fmap f $ _piece st }
    ghostIdx <- gets _ghostPieceIndex >>= \case
        Just i -> return i
        Nothing -> do
            whileM_ (tryMove Init Down) $ return ()
            gets _pieceIndex
    ghost <- gets _piece
    return (ghost, ghostIdx)


getCroppedGhost :: (GameContext p u m) => (u -> u) -> GameEngine p u m (p u, Index)
getCroppedGhost f = do
    pieceIndex <- gets _pieceIndex
    piece <- gets _piece
    (ghost, ghostIndex) <- getGhost f
    let offset = subtractIndex ghostIndex pieceIndex
        pieceGrid = getGrid piece
        ghost' = overlayBy2 g offset pieceGrid ghost
        g _ _ = Empty
    return (ghost', ghostIndex)


getRows :: (GameContext p u m) => GameEngine p u m [[Cell u]]
getRows = do
    dim <- gets $ _playfieldDim . _config
    field <- gets _field
    let (_, h) = dim
        idxs = [0 .. h - 1]
        rows = map (`Field.getRow` field) idxs
    return rows


isFull :: [Cell a] -> Bool
isFull = and . map f
    where
        f Empty = False
        f _ = True


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
    gets _field >>= prompt . PlayfieldChanged
    updateLevel


clearRow :: (GameContext p u m) => Int -> GameEngine p u m ()
clearRow idx = do
    field <- gets $ Field.clearRow idx . _field
    rowsCleared <- gets _rowsCleared
    let field' = Field.dropRowsAbove idx field
        rowsCleared' = rowsCleared + 1
    modify $ \st -> st {
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


updateLevel :: (GameContext p u m) => GameEngine p u m ()
updateLevel = do
    rowsCleared <- gets _rowsCleared
    level <- gets _level
    let level' = Level $ (rowsCleared `div` 10) + 1
    case level == level' of
        True -> return ()
        False -> do
            modify $ \st -> st { _level = level }
            prompt $ LevelChanged level'


calculatePoints :: Level -> RowCount -> Score
calculatePoints level = Score . (unLevel level *) . \case
    1 -> 100
    2 -> 300
    3 -> 500
    4 -> 800
    n -> error $ "Don't know how to score " ++ show n ++ " rows cleared."












