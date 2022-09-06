{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Executor where
import Parser
    ( Rule(Rule), Atom(..), Header(..), Program(..), parse )
import Graphics.Gloss ( black, color, Picture(Line, Pictures) )
import Data.List (find)
import Control.Monad.State.Lazy (State, MonadState (get, put), runState)
import qualified Data.Map as M
import Text.Parsec (ParseError)
import qualified Lex
import Control.Monad (forM_)
import Control.Monad.Reader (ReaderT (runReaderT), MonadReader (ask))

-- F Draw forward
-- B Draw backward
-- f Move forward
-- b Move backward
-- + Rotate right
-- - Rotate left
-- [ Push position
-- ] Pop position

getAxiom :: [Header] -> [Atom]
getAxiom hds = do
    case (find (\case
        (Axiom _) -> True
        _ -> False) hds) of
            Just (Axiom atoms) -> atoms
            _ -> []

getAngle :: [Header] -> Float
getAngle hds = do
    case (find (\case
        (Angle _) -> True
        _ -> False) hds) of
            Just (Angle a) -> (realToFrac a) * pi/180
            _ -> 0

runProg :: Program -> RunOptions -> (RunState, [Atom], Picture)
runProg (Program hds rules) options = do
    let initAtoms = getAxiom hds
        progAngle = getAngle hds
        rulesMap = foldr (\(Rule a atoms) r -> (M.insert a atoms r)) (M.empty) rules
        expanded = runGen initAtoms rulesMap (numberOfGeneration options)
        initState = RunState {
            currentAngle = pi/2,
            posStack = [],
            paths = [],
            coord = (0, 0),
            debug = []
        }
        (_, finalState) = runState (runReaderT (forM_ expanded (\atom -> processAtom atom)) (options {unitAngle=progAngle})) initState
    (finalState, expanded, Pictures $ map (color black) (paths finalState))
    
runGen :: [Atom] -> M.Map Atom [Atom] -> Integer -> [Atom]
runGen atoms rules generation
    | generation > 0 = do
        let newGen = foldMap (\atom -> maybe ([atom]) (\as -> as) (M.lookup atom rules)) atoms
        runGen newGen rules (generation - 1)
    | otherwise = atoms
    
data RunState = RunState {
    currentAngle :: Float,
    coord :: (Float, Float),
    paths :: [Picture],
    posStack :: [(Float, (Float, Float))],
    debug :: [String]
} deriving (Show)

processAtom :: Atom -> ReaderT RunOptions (State RunState) ()
processAtom atom 
    | (Symb f) <- atom =
        case f of    
            "B" -> drawBackward
            "b" -> moveBackward
            "F" -> drawForward
            "f" -> moveForward
            "-" -> rotateRight
            "+" -> rotateLeft
            "[" -> pushPosition
            "]" -> popPosition
    | (Id (a:[])) <- atom = do
        options <- ask
        if (a `elem` (drawForwardSymbol options)) then drawForward
        else if (a `elem` (drawBackwardSymbol options)) then drawBackward
        else return ()

drawBackward :: ReaderT RunOptions (State RunState) ()
drawBackward = do
    state <- get
    options <- ask
    let (x, y) = coord state
        (newX, newY) = (x - ((unitLength options) * cos (currentAngle state)), y - ((unitLength options) * sin (currentAngle state)))
        p = Line [( (newX),  (newY)), ( x,  y) ]
    put (state {coord = (newX, newY), paths=(p:(paths state)), debug=((debug state) ++ ["drawBackward:"++(mconcat ["(",show newX,",", show newY,")"])])})

drawForward ::  ReaderT RunOptions (State RunState) ()
drawForward = do
    state <- get
    options <- ask
    let (x, y) = coord state
        (newX, newY) = (x + ((unitLength options) * cos (currentAngle state)), y + ((unitLength options) * sin (currentAngle state)))
        p = Line [( x,  y), ( (newX),  (newY))]
    put (state {coord = (newX, newY), paths=(p:(paths state)), debug=((debug state) ++ ["drawForward:"++(mconcat ["(",show newX,",", show newY,")"])])})
    
moveBackward :: ReaderT RunOptions (State RunState) ()
moveBackward = do
    state <- get
    options <- ask
    let (x, y) = coord state
        (newX, newY) = (x - ((unitLength options) * cos (currentAngle state)), y - ((unitLength options) * sin (currentAngle state)))
    put (state {coord = (newX, newY), debug=((debug state) ++ ["moveBackward"])})
    
moveForward :: ReaderT RunOptions (State RunState) ()
moveForward = do
    state <- get
    options <- ask
    let (x, y) = coord state
        (newX, newY) = (x + ((unitLength options) * cos (currentAngle state)), y + ((unitLength options) * sin (currentAngle state)))
    put (state {coord = (newX, newY), debug=((debug state) ++ ["moveForward"])})

rotateRight :: ReaderT RunOptions (State RunState) ()
rotateRight = do
    state <- get
    options <- ask
    let newAngle = (currentAngle state) - (unitAngle options)
    put(state {currentAngle = newAngle, debug=((debug state) ++ ["rotateRight:"++(show newAngle)])})

rotateLeft :: ReaderT RunOptions (State RunState) ()
rotateLeft = do
    state <- get
    options <- ask
    let newAngle = (currentAngle state) + (unitAngle options)
    put(state {currentAngle = newAngle, debug=((debug state) ++ ["rotateLeft:"++(show newAngle)])})

pushPosition :: ReaderT RunOptions (State RunState) ()
pushPosition = do
    state <- get
    let currentPosStack = posStack state
    put (state {posStack = ((currentAngle state, coord state) : currentPosStack)})

popPosition :: ReaderT RunOptions (State RunState) ()
popPosition = do
    state <- get
    case posStack state of
        [] -> return ()
        ((sAngle, sCoord) : rst) -> put (state {currentAngle = sAngle, coord = sCoord, posStack = rst})

data RunOptions = RunOptions {
    unitLength :: Float,
    unitAngle :: Float,
    numberOfGeneration :: Integer,
    drawForwardSymbol :: String,
    drawBackwardSymbol :: String,
    debugMode :: Bool
}  deriving (Show)
    
exec :: String -> RunOptions -> Either ParseError (RunState, [Atom], Picture)
exec str options = do
    toks <- (Lex.lex str)
    prog <- parse toks
    return $ runProg prog options
    