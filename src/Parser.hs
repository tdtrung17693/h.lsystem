{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Parser where
import Text.Parsec
import Lex

data Program = Program [Header] [Rule]
    deriving (Show, Eq)
data Header = Axiom [AtomFnCall] | Angle Double
    deriving (Show, Eq)

type Op = String
data Expr =
    BinOp Op Expr Expr
    | FnCall Atom [Expr]
    | AtomRef Atom
    | Number Float
    deriving (Show, Eq, Ord)
data Atom = Atom String
    deriving (Show, Eq, Ord)
data AtomFn = AtomFn Atom [Atom]
    deriving (Show, Eq, Ord)

data AtomFnCall = AtomFnCall Atom [Expr]
    deriving (Show, Eq)
data Rule = Rule AtomFn [AtomFnCall]
    deriving (Show, Eq)

advance :: SourcePos -> t -> [TokenPos] -> SourcePos
advance _ _ ((_, pos) : _) = pos
advance pos _ [] = pos

satisfyP :: (TokenPos -> Bool) -> Parsec [TokenPos] st Token
satisfyP f = tokenPrim show
                      advance
                      (\c -> if f c then Just (fst c) else Nothing)

parse :: [TokenPos] -> Either ParseError Program
parse toks = runParser (do
    hds <- try (many header)
    rules <- many1 rule
    return $ Program hds rules) () "" toks

debugParser :: [TokenPos] -> IO ()
debugParser toks = parseTest ((do
    hds <- try (many header)
    rules <- many1 rule
    return $ Program hds rules)) toks

header :: Parsec [TokenPos] st Header 
header = (axiom <|> angle)  <* (many (satisfyP (\t -> fst t == TSep)))

angle :: Parsec [TokenPos] st Header
angle = do
    _ <- satisfyP (\case 
        (TAngle,_) -> True
        _ -> False)
    (TNumber n) <- (satisfyP (\case 
        (TNumber _,_) -> True
        _ -> False))
    return $ Angle n

axiom :: Parsec [TokenPos] st Header
axiom = do
    _ <- satisfyP (\case 
        (TAxiom, _) -> True
        _ -> False)
    toks <- manyTill atomFnCall (lit TSep)
    return $ Axiom toks

lit :: Token -> Parsec [TokenPos] st Token
lit a = satisfyP (\s -> fst s == a)

atom :: Parsec [TokenPos] st Atom
atom = do
    atomInstance <- (satisfyP (\case 
        (TId _, _) -> True
        (TSym _, _) -> True
        _ -> False))
    return $ mapToken atomInstance
    where mapToken (TId i) = Atom i
          mapToken (TSym s) = Atom s

atomFn :: Parsec [TokenPos] st AtomFn
atomFn = (try $ do
    atomIns <- atom
    _ <- lit TLParen
    params <- sepBy (atom) (lit TComma)
    _ <- lit TRParen
    return $ AtomFn atomIns params)
    <|> do
    atomIns <- atom
    return $ AtomFn atomIns []


numberExpr :: Parsec [TokenPos] st Expr
numberExpr = do
    (TNumber n) <- satisfyP (\case
        (TNumber _, _) -> True
        _ -> False)
    return $ Number (realToFrac n)

multOp :: Parsec [TokenPos] st Token
multOp = satisfyP (\case
            (TSym a, _) -> if a `elem` ["*","/"] then True else False 
            _ -> False)

addtOp :: Parsec [TokenPos] st Token
addtOp = satisfyP (\case
            (TSym a, _) -> if a `elem` ["+","-"] then True else False 
            _ -> False)

expr :: Parsec [TokenPos] st Expr
expr = do
    tm <- term <?> "expr.term"
    e' <- try (expr' <?> "expr.expr'") <|> return ANull
    case e' of
        ANull -> return $ tm
        A (o, ex, at) -> return $ BinOp o tm (mapA ex at)
    where mapA op1 (A (o, op2, at)) = BinOp o op1 (mapA op2 at)
          mapA op1 ANull = op1

expr' :: Parsec [TokenPos] st A
expr' = do
    (TSym o) <- addtOp <?> "expr'.addtOp"
    t <- term <?> "expr'.term"
    e' <- (try (expr' <?> "expr'.expr'")) <|> return ANull
    return $ A (o, t, e')

data A = A (Op, Expr, A) | ANull

term :: Parsec [TokenPos] st Expr
term = do
    ft <- factor <?> "term:factor."
    t' <- (try (term' <?> "term:term'")) <|> return ANull
    case t' of
        ANull -> return $ ft
        A (o, ex, at) -> return $ BinOp o ft (mapA ex at)
    where mapA op1 (A (o, op2, at)) = BinOp o op1 (mapA op2 at)
          mapA op1 ANull = op1

term' :: Parsec [TokenPos] st A
term' = do
    (TSym o) <- multOp <?> "term'.multOp"
    ft <- factor <?> "term'.factor"
    t <- (try (term' <?> "term'.term'")) <|> return ANull
    return $ A (o, ft, t)
     
factor :: Parsec [TokenPos] st Expr
factor = (atomExpr <|> numberExpr) <?> "factor"
    
atomExpr :: Parsec [TokenPos] st Expr
atomExpr = do
    atomIns <- atom <?> "atomExpr.atom"
    params <- argList <|> return [] 
    case params of
        [] -> return $ AtomRef atomIns
        _ -> return $ FnCall atomIns params

atomFnCall :: Parsec [TokenPos] st AtomFnCall
atomFnCall = do
    atomIns <- atom <?> "atomExpr.atom"
    params <- argList <|> return [] 
    return $ AtomFnCall atomIns params

argList :: Parsec [TokenPos] st [Expr]
argList = do
    _ <- lit TLParen
    params <- sepBy (expr) (lit TComma)
    _ <- lit TRParen
    return $ params

rule :: Parsec [TokenPos] st Rule
rule = (do
    ruleName <- atomFn
    _ <- lit TArrow
    toks <- manyTill atomFnCall (lit TSep)
    return $ Rule (ruleName) toks)
