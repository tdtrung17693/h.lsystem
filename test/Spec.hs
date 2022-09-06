import Test.Hspec (hspec, describe)
import LexerSpec (lexerSpec)
import ParserSpec (parserSpec)
main :: IO ()
main = hspec $ do
    describe "Lexer spec" $ lexerSpec
    describe "Parser spec" $ parserSpec

