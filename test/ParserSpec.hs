module ParserSpec(parserSpec) where
import Test.Hspec ( it, shouldBe, SpecWith, Arg, Expectation )
import Lex 
import Parser 
import Control.Monad (liftM2)

parserSpec :: SpecWith ()
parserSpec =  do
    it "can parse rule A -> B-A-B" $ do
        let toks = Lex.lex "A -> B-A-B"
        ((parse <$> toks)) `shouldBe` (Right (Right (Program [] ([Rule (Id "A") [Symb "B", Symb "-", Id "A", Symb "-", Symb "B"]]))))

    it "can parse rule A -> B-A-B\n B -> A+B+A" $ do
        let toks = Lex.lex "A -> B-A-B\n B -> A+B+A"
        ((parse <$> toks)) `shouldBe` (Right (Right (
            Program [] ([
                Rule (Id "A") [Symb "B", Symb "-", Id "A", Symb "-", Symb "B"],
                Rule (Symb "B") [Id "A", Symb "+", Symb "B", Symb "+", Id "A"]]))))
    
    -- it "can parse axiom and rule axiom A\nA -> B-A-B\n B -> A+B+A" $ do
    --     parse [TAxiom, TId "A", TSep,TId "A", TArrow,
    --         TId "B", TSym "-", TId "A", TSym "-", TId "B", TSep,
    --         TId "B", TArrow,
    --         TId "A", TSym "+", TId "B", TSym "+", TId "A", TSep] `shouldBe` 
    --         (Right (Program [Axiom [Id "A"]] (
    --             [Rule (Id "A") [Id "B", Symb "-", Id "A", Symb "-", Id "B"],
    --             Rule (Id "B") [Id "A", Symb "+", Id "B", Symb "+", Id "A"]])))
