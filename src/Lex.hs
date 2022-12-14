module Lex where
import Data.Functor ( ($>) )
import Text.ParserCombinators.Parsec
    ( ParseError,
      alphaNum,
      char,
      digit,
      spaces,
      string,
      oneOf,
      many1,
      (<|>),
      runParser,
      try, skipMany, choice, getPosition, manyTill )
import Text.Parsec (Parsec, endOfLine, SourcePos, eof)
import Text.Parsec.Pos (initialPos)
import Data.Char (isSpace)

data Token = 
    TAxiom
    | TAngle
    | TAtom
    | TArrow
    | TNumber Double
    | TLParen
    | TRParen
    | TId String
    | TSym String
    | TSep
    | TComma
    deriving (Show, Eq)
type TokenPos = (Token, SourcePos)
lex :: String -> Either ParseError [TokenPos]
lex str = (runParser ((spaces *> (manyTill (tok) eof ))) () "") processedStr
    where processedStr = (unlines nonBlankLines)
          nonBlankLines = filter (not . all isSpace) $ lines str

tok :: Parsec String st TokenPos
tok = 
    (,) <$> (tSep <|>
    (try tAxiom)
    <|> tAngle
    <|> (try tArrow)
    <|> tSym
    <|> tLParen
    <|> tRParen
    <|> tComma
    <|> tNumber
    <|> tId) <* (skipMany  (choice [char ' ', char '\r', char '\t'])) <*> getPosition

tSep :: Parsec String st Token
tSep = (try eof <|> (endOfLine $> ())) $> TSep

tAxiom :: Parsec String st Token
tAxiom = string "axiom" $> TAxiom

tAngle ::  Parsec String st Token
tAngle = string "angle" $> TAngle

tNumber :: Parsec String st Token
tNumber = do
    whole  <- many1 digit
    decimal <- (try (char '.') >> (many1 digit)) <|> string ""

    return $ TNumber (read (mconcat [whole, if decimal /= "" then  "." ++ decimal else ""]) :: Double)

tArrow :: Parsec String st Token
tArrow = string "->" $> TArrow

tSym :: Parsec String st Token
tSym = oneOf "+-/*[]^\\|{}#!;" >>= \s -> return $ TSym [s]

tId :: Parsec String st Token
tId = alphaNum >>= \s -> return $ TId [s]

tComma :: Parsec String st Token
tComma = char ',' $> TComma

tLParen :: Parsec String st Token
tLParen = char '(' $> TLParen

tRParen :: Parsec String st Token
tRParen = char ')' $> TRParen
