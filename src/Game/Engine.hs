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
    playGame,
) where


import Control.Monad.Loops (whileM_)
import Control.Monad.Prompt
import Control.Monad.State.Lazy
import Data.Cell
import Data.Grid (Index, Dimensions)
import qualified Data.Grid as Grid
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NonEmpty
import Data.Maybe (mapMaybe)
import Data.Ratio
import Data.Rotate (RotateDir(..))
import Data.Stream (Stream)
import qualified Data.Stream as Stream
import Game.Gravity
import Game.Level
import Game.Piece (Piece)
import qualified Game.Piece as Piece
import Game.Playfield (Playfield)
import qualified Game.Playfield as Field
import Prelude hiding (Left, Right, foldr)


branch :: (MonadState s m) => (s -> s) -> m a -> m a
branch f m = do
    st <- get
    modify f
    x <- m
    put st
    return x


subtractIndex :: Index -> Index -> Index
subtractIndex (x, y) (x', y') = (x - x', y - y')


type LockAction u = u -> u
type RowCount = Int
type TotalRowCount = RowCount


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
    deriving (Eq, Ord, Num)


instance Show Score where
    show = show . unScore


data GamePrompt u :: * -> * where
    GetAction :: GamePrompt u Action
    ScoreChanged :: Score -> GamePrompt u ()
    LevelChanged :: Level -> GamePrompt u ()
    PlayfieldChanged :: Playfield u -> GamePrompt u ()
    RowsCleared :: NonEmpty Int -> TotalRowCount -> GamePrompt u ()


defaultPrompt :: (Monad m) => GamePrompt u a -> GameEngine p u m a
defaultPrompt = \case
    GetAction {} -> return DoNothing
    ScoreChanged {} -> return ()
    LevelChanged {} -> return ()
    PlayfieldChanged {} -> return ()
    RowsCleared {} -> return ()


data GameConfig u = GameConfig {
    _ghostify :: Maybe (u -> u),
    _lockAction :: u -> u,
    _gravityRate :: Level -> Gravity,
    _playfieldDim :: Dimensions
}


defaultGameConfig :: GameConfig u
defaultGameConfig = GameConfig {
    _ghostify = Nothing,
    _lockAction = id,
    _gravityRate = const $ 1 / 60,
    _playfieldDim = (10, 22)
}


data GameState p u = GameState {
    _promptEnabled :: Bool,
    _config :: GameConfig u,
    _field :: Playfield u,
    _piece :: p u,
    _pieceIndex :: Index,
    _ghostPieceIndex :: Maybe Index,
    _futurePieces :: Stream (p u),
    _rowsCleared :: TotalRowCount,
    _level :: Level,
    _score :: Score,
    _gravityProgress :: Gravity,
    _gameOver :: Bool
}


mkGameState :: GameConfig u -> Stream (p u) -> GameState p u
mkGameState config pieces = GameState {
    _promptEnabled = True,
    _config = config,
    _field = Field.mkPlayfield $ _playfieldDim config,
    _piece = Stream.head pieces,
    _pieceIndex = (0, 0),
    _ghostPieceIndex = Nothing,
    _futurePieces = pieces,
    _level = level,
    _rowsCleared = 0,
    _score = 0,
    _gravityProgress = 0,
    _gameOver = False }
    where
        level = 1


newtype GameEngine p u m a = GameEngine {
    unGameEngine :: StateT (GameState p u) m a
} deriving (Monad, MonadState (GameState p u), MonadTrans, MonadIO)


type GameMonad u m = (MonadPrompt (GamePrompt u) m)
type GameContext p u m = (GameMonad u m, Piece p, Functor p)


instance (GameMonad u m) => MonadPrompt (GamePrompt u) (GameEngine p u m) where
    prompt p = gets _promptEnabled >>= \case
        False -> defaultPrompt p
        True -> lift $ prompt p


playGame :: (GameContext p u m) => GameConfig u -> Stream (p u) -> m Score
playGame config pieces = liftM _score $ flip execStateT st $ unGameEngine runGame
    where
        st = mkGameState config pieces


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
tickGame = tickFrame


tickFrame :: (GameContext p u m) => GameEngine p u m ()
tickFrame = do
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
    let grid = Piece.getGrid p
        (pw, _) = Grid.dimensions grid
        (gw, _) = dim
        startIdx = ((gw - pw) `div` 2, 0)
    case Field.addPiece startIdx p field of
        Nothing -> gameOver
        Just field' -> do
            modify $ \st -> st {
                _pieceIndex = startIdx,
                _field = field' }
            addGhostPiece


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


bringUnder1 :: Gravity -> (Gravity, Int)
bringUnder1 g = (g', n)
    where
        r = unGravity g
        numer = numerator r
        denom = denominator r
        (n, numer') = numer `divMod` denom
        g' = Gravity $ (numer' % denom)


tickGravity :: (GameContext p u m) => GameEngine p u m ()
tickGravity = do
    level <- gets _level
    rate <- gets $ _gravityRate . _config
    progress <- gets $ (rate level +) . _gravityProgress
    let (progress', cellsToDrop) = bringUnder1 progress
    modify $ \st -> st { _gravityProgress = progress' }
    dropResults <- replicateM cellsToDrop dropPiece
    case and dropResults of
        True -> return ()
        False -> initLockDelay


dropPiece :: (GameContext p u m) => GameEngine p u m Bool
dropPiece = tryMove Init Down


initLockDelay :: (GameContext p u m) => GameEngine p u m ()
initLockDelay = do
    lockPiece -- TODO: Do this properly


lockPiece :: (GameContext p u m) => GameEngine p u m ()
lockPiece = do
    f <- gets $ _lockAction . _config
    modify $ \st -> st { _piece = fmap f $ _piece st }
    field <- gets _field
    prompt $ PlayfieldChanged field
    tryClearRows
    nextPiece


tryRotate :: (GameContext p u m) => RotateDir -> GameEngine p u m ()
tryRotate dir = do
    removeGhostPiece
    p <- gets _piece
    let p' = Piece.rotate dir p
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
addGhostPiece = gets (_ghostify . _config) >>= \case
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
removeGhostPiece = gets (_ghostify . _config) >>= \case
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
        config' = config { _ghostify = Nothing }
        piece = _piece st
        ghost = fmap f piece
        in st {
            _promptEnabled = False,
            _config = config',
            _piece = ghost }
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
    let offset = subtractIndex pieceIndex ghostIndex
        pieceGrid = Piece.getGrid piece
        ghost' = Piece.overlayBy2 g offset pieceGrid ghost
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
    mapM_ clearRow $ NonEmpty.toList idxs
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












