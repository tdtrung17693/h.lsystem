{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Parser where
import Text.Parsec
import Lex

data Program = Program [Header] [Rule]
    deriving (Show, Eq)
data Header = Axiom [Atom] | Angle Double
    deriving (Show, Eq)
data Atom = Symb String | Id String
    deriving (Show, Eq, Ord)
data Rule = Rule Atom [Atom]
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
    toks <- many (satisfyP (\case 
        (TId _, _) -> True
        (TSym _, _) -> True
        _ -> False))
    let atoms = map mapToken toks
    return $ Axiom atoms
    where mapToken (TId i) = Id i
          mapToken (TSym s) = Symb s
    

rule :: Parsec [TokenPos] st Rule
rule = (do
    l <- satisfyP (\case 
        (TId _, _) -> True
        (TSym _, _) -> True
        _ -> False)
    _ <- satisfyP (\t -> fst t == TArrow) 
    toks <- manyTill (satisfyP (\case 
        (TId _, _) -> True
        (TSym _, _) -> True
        _ -> False)) ((satisfyP (\t -> fst t == TSep)))
    _ <- many (satisfyP (\t -> fst t == TSep))
    let atoms = map mapToken toks
    return $ Rule (mapToken l) atoms)
    where mapToken (TId i) = Id i
          mapToken (TSym s) = Symb s
