module ParserSpec(parserSpec) where
import Test.Hspec ( it, shouldBe, SpecWith, Arg, Expectation )
import Lex 
import Parser 
import Control.Monad (liftM2)
import Parser (AtomFn(AtomFn), Expr (FnCall), debugParser)
import Parser
import Control.Monad.Cont

parserSpec :: SpecWith ()
parserSpec =  do
    it "can parse rule A -> B-A-B" $ do
        let toks = Lex.lex "A -> B-A-B"
        ((parse <$> toks)) `shouldBe` (Right (Right (Program [] ([Rule (AtomFn (Atom "A") []) [
            AtomFnCall (Atom "B") [],
            AtomFnCall ((Atom "-")) [],
            AtomFnCall ((Atom "A")) [],
            AtomFnCall ((Atom "-")) [],
            AtomFnCall ((Atom "B")) []]]))))

    it "can parse rule A -> B-A-B\n B -> A+B+A" $ do
        let toks = Lex.lex "A -> B-A-B\n B -> A+B+A"
        ((parse <$> toks)) `shouldBe` (Right (Right (
            Program [] ([
                Rule (AtomFn (Atom "A") []) [
                    AtomFnCall ((Atom "B")) [],
                    AtomFnCall ((Atom "-")) [],
                    AtomFnCall ((Atom "A")) [],
                    AtomFnCall ((Atom "-")) [],
                    AtomFnCall ((Atom "B")) []],
                Rule (AtomFn (Atom "B") []) [
                    AtomFnCall ((Atom "A")) [],
                    AtomFnCall ((Atom "+")) [],
                    AtomFnCall ((Atom "B")) [],
                    AtomFnCall ((Atom "+")) [],
                    AtomFnCall ((Atom "A")) []]]))))

    it "can parse rule Y -> FX" $ do
        let toks = Lex.lex "Y -> FX"
        
        ((parse <$> toks)) `shouldBe` (Right (Right (
            Program [] ([
                Rule (AtomFn (Atom "Y") []) [
                    AtomFnCall ((Atom "F")) [],
                    AtomFnCall ((Atom "X")) []]]))))

    it "can parse rule Y(l) -> F(l)" $ do
        let toks = Lex.lex "Y(l) -> F(l)"
        
        ((parse <$> toks)) `shouldBe` (Right (Right (
            Program [] ([
                Rule (AtomFn (Atom "Y") [Atom "l"]) [
                    AtomFnCall (Atom "F") [AtomRef (Atom "l")]]]))))

    it "can parse rule Y(l) -> F(l)X(l)" $ do
        let toks = Lex.lex "Y(l) -> F(l)X(l)"
        
        ((parse <$> toks)) `shouldBe` (Right (Right (
            Program [] ([
                Rule (AtomFn (Atom "Y") [Atom "l"]) [
                    AtomFnCall (Atom "F") [AtomRef (Atom "l")],
                    AtomFnCall (Atom "X") [AtomRef (Atom "l")]]]))))

    it "can parse rule Y(l) -> F(l/2)" $ do
        let toks = Lex.lex "Y(l) -> F(l/2)"
        
        ((parse <$> toks)) `shouldBe` (Right (Right (
            Program [] ([
                Rule (AtomFn (Atom "Y") [Atom "l"]) [
                    AtomFnCall (Atom "F") [BinOp "/" (AtomRef (Atom "l")) (Number 2)]]]))))

    it "can parse rule Y(l) -> F(l/2 + 1)" $ do
        let toks = Lex.lex "Y(l) -> F(l/2 + 1)"
        
        ((parse <$> toks)) `shouldBe` (Right (Right (
            Program [] ([
                Rule (AtomFn (Atom "Y") [Atom "l"]) [
                    AtomFnCall (Atom "F") [BinOp "+" (BinOp "/" (AtomRef (Atom "l")) (Number 2)) (Number 1)]]]))))
    