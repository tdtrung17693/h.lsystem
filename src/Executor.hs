{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Executor where
import Parser
    ( Rule(Rule), Atom(..), Header(..), Program(..), parse, Expr (FnCall, BinOp, Number, AtomRef), AtomFn (AtomFn), Op, AtomFnCall (AtomFnCall) )
import Graphics.Gloss ( black, color, Picture(Line, Pictures) )
import Control.Monad.State.Lazy (State, MonadState (get, put), runState)
import qualified Data.Map as M
import Text.Parsec (ParseError)
import qualified Lex
import Control.Monad.Reader (ReaderT (runReaderT), MonadReader (ask))
import Data.Foldable
import Data.Maybe (fromMaybe)

-- F Draw forward
-- B Draw backward
-- f Move forward
-- b Move backward
-- + Rotate right
-- - Rotate left
-- [ Push position
-- ] Pop position

getAxiom :: [Header] -> [AtomFnCall]
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

runProg :: Program -> RunOptions -> (RunState, RunOptions, [AtomFnCall], Picture)
runProg (Program hds rules) options = do
    let initAtoms = getAxiom hds
        progAngle = getAngle hds
        rulesMap = foldr (\(Rule atomFn@(AtomFn atom _) atoms) r -> (M.insert atom (atomFn ,atoms) r)) (M.empty) rules
        expanded = (runGen initAtoms rulesMap (numberOfGeneration options))
        initState = RunState {
            currentAngle = 0,
            posStack = [],
            paths = [],
            coord = (0, 0),
            debug = [],
            callStack = []
        }
        (_, finalState) = runState (runReaderT (forM_ (expanded) (\atom -> processAtom atom)) (options {unitAngle=progAngle})) initState
    (finalState, options {unitAngle=progAngle}, expanded, Pictures $ map (color black) (paths finalState))

expandFn :: AtomFnCall -> M.Map Atom (AtomFn, [AtomFnCall]) -> Maybe [AtomFnCall]
expandFn (AtomFnCall (Atom a) args) rules = do
    ((AtomFn _ atoms), ruleExpr) <- M.lookup (Atom a) rules
    return $  (map (\f -> substituteFnParam f atoms args) ruleExpr)

substituteFnParam :: AtomFnCall -> [Atom] -> [Expr] -> AtomFnCall
substituteFnParam (AtomFnCall (Atom fnName) params) atoms args =
    AtomFnCall (Atom fnName) (foldMap (\(atom, expr) -> substitute atom expr <$> params) (zip atoms args)) 

substitute :: Atom -> Expr -> Expr -> Expr
substitute (Atom a) expr (AtomRef (Atom b)) = if a == b then expr else AtomRef (Atom b)
substitute _ _ n@(Number _) = n
substitute atom expr (BinOp op atom1 atom2) = BinOp op (substitute atom expr atom1) (substitute atom expr atom2)
substitute atom expr (FnCall atomfn exprs) = FnCall atomfn (map (substitute atom expr) exprs)

expandRules :: M.Map Atom (AtomFn, [AtomFnCall]) -> M.Map Atom (AtomFn, [AtomFnCall])
expandRules rules = do
    M.map (\(atomFn, atomFnCalls) -> (atomFn,foldMap (\a -> (fromMaybe ([a]) (expandFn a rules))) atomFnCalls)) rules

runGen :: [AtomFnCall] -> M.Map Atom (AtomFn, [AtomFnCall]) -> Integer -> [AtomFnCall]
runGen atomFnCalls rules generation
    | generation > 0 = do
        let newGen = foldMap (\aFnCall -> maybe ([aFnCall]) (\(exprs)-> exprs) (expandFn aFnCall rules)) atomFnCalls
        runGen newGen (rules) (generation - 1)
    | otherwise = atomFnCalls
    
data RunState = RunState {
    currentAngle :: Float,
    coord :: (Float, Float),
    paths :: [Picture],
    posStack :: [(Float, (Float, Float))],
    debug :: [String],
    callStack :: [(M.Map Atom Atom)]
} deriving (Show)

processAtom :: AtomFnCall -> ReaderT RunOptions (State RunState) ()
processAtom atom 
    | (AtomFnCall (Atom (a:_)) args) <- atom = do
        options <- ask
        let values = execExpr <$> args
        let value = case values of
                    [] -> 1
                    (v:_) -> v
        if (a `elem` (drawForwardSymbol options)) then drawForward value
        else if (a `elem` (drawBackwardSymbol options)) then drawBackward value
        else case a of
            'b' -> moveBackward value
            'f' -> moveForward value
            '-' -> rotateRight value
            '+' -> rotateLeft value
            '[' -> pushPosition
            ']' -> popPosition
            _ -> return ()

execExpr :: Expr -> Float
execExpr expr = case expr of
    (FnCall _ _) -> 1
    (BinOp o op1 op2) -> case o of
        "+" -> (execExpr op1) + (execExpr op2)
        "-" -> (execExpr op1) - (execExpr op2)
        "*" -> (execExpr op1) * (execExpr op2)
        "/" -> (execExpr op1) / (execExpr op2)
    (Number n) -> n

drawBackward :: Float -> ReaderT RunOptions (State RunState) ()
drawBackward l = do
    state <- get
    options <- ask
    let (x, y) = coord state
        (newX, newY) = (x - (l*(unitLength options) * cos (currentAngle state)), y - (l*(unitLength options) * sin (currentAngle state)))
        p = Line [( (newX),  (newY)), ( x,  y) ]
    put (state {coord = (newX, newY), paths=(p:(paths state)), debug=((debug state) ++ ["drawBackward("++(mconcat [show l, "):", "(",show newX,",", show newY,")"])])})

drawForward :: Float ->  ReaderT RunOptions (State RunState) ()
drawForward l = do
    state <- get
    options <- ask
    let (x, y) = coord state
        (newX, newY) = (x + (l * (unitLength options) * cos (currentAngle state)), y + (l*(unitLength options) * sin (currentAngle state)))
        p = Line [( x,  y), ( (newX),  (newY))]
    put (state {coord = (newX, newY), paths=(p:(paths state)), debug=((debug state) ++ ["drawForward("++(mconcat [show l, "):", "(",show newX,",", show newY,")"])])})
    
moveBackward :: Float -> ReaderT RunOptions (State RunState) ()
moveBackward l = do
    state <- get
    options <- ask
    let (x, y) = coord state
        (newX, newY) = (x - (l * (unitLength options) * cos (currentAngle state)), y - (l * (unitLength options) * sin (currentAngle state)))
    put (state {coord = (newX, newY), debug=((debug state) ++ ["moveBackward("++(mconcat [show l, ")"])])})
    
moveForward :: Float -> ReaderT RunOptions (State RunState) ()
moveForward l = do
    state <- get
    options <- ask
    let (x, y) = coord state
        (newX, newY) = (x + (l*(unitLength options) * cos (currentAngle state)), y + (l*(unitLength options) * sin (currentAngle state)))
    put (state {coord = (newX, newY), debug=((debug state) ++ ["moveForward("++(mconcat [show l, ")"])])})

rotateRight :: Float -> ReaderT RunOptions (State RunState) ()
rotateRight a = do
    state <- get
    options <- ask
    let newAngle = (currentAngle state) - a * (unitAngle options)
    put(state {currentAngle = newAngle, debug=((debug state) ++ ["rotateRight("++(mconcat [(show a), "):",(show newAngle)])])})

rotateLeft :: Float -> ReaderT RunOptions (State RunState) ()
rotateLeft a = do
    state <- get
    options <- ask
    let newAngle = (currentAngle state) + a * (unitAngle options)
    put(state {currentAngle = newAngle, debug=((debug state) ++ ["rotateLeft("++(mconcat [show a, "):", show newAngle])])})

pushPosition :: ReaderT RunOptions (State RunState) ()
pushPosition = do
    state <- get
    let currentPosStack = posStack state
    put (state {
            posStack = ((currentAngle state, coord state) : currentPosStack),
            debug=((debug state) ++ [(mconcat ["pushPosition:(", show $ currentAngle state, "[",show $ coord state, "]", ")"])])
        })

popPosition :: ReaderT RunOptions (State RunState) ()
popPosition = do
    state <- get
    case posStack state of
        [] -> return ()
        ((sAngle, sCoord) : rst) -> put (state {
            currentAngle = sAngle, coord = sCoord, posStack = rst,
            debug=((debug state) ++ [(mconcat ["popPosition:(", show $ sAngle, "[",show sCoord, "]", ")"])])})

data RunOptions = RunOptions {
    unitLength :: Float,
    unitAngle :: Float,
    numberOfGeneration :: Integer,
    drawForwardSymbol :: String,
    drawBackwardSymbol :: String,
    debugMode :: Bool
}  deriving (Show)
    
exec :: String -> RunOptions -> Either ParseError (RunState, RunOptions, [AtomFnCall], Picture)
exec str options = do
    toks <- (Lex.lex str)
    prog <- parse toks
    return $ runProg prog (initDefault options)
    
initDefault :: RunOptions -> RunOptions
initDefault options = options {
    drawBackwardSymbol = 'B':(drawBackwardSymbol options),
    drawForwardSymbol = 'F':(drawForwardSymbol options)
}
