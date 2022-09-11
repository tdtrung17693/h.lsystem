module LexerSpec(lexerSpec) where
import Test.Hspec ( it, shouldBe, SpecWith )
import Lex

lexerSpec :: SpecWith ()
lexerSpec =  do
    it "can scan axiom" $ do
        fmap (map fst) (Lex.lex "axiom X") `shouldBe` (Right [TAxiom, TId "X", TSep])
    it "can scan angle" $ do
        fmap (map fst) (Lex.lex "angle 90") `shouldBe` (Right [TAngle, TNumber 90.0, TSep])
    it "can scan both axiom and angle" $ do
        fmap (map fst) (Lex.lex "axiom X\nangle 90" )`shouldBe` (Right [TAxiom, TId "X", TSep, TAngle, TNumber 90, TSep])
    it "can scan a rule" $ do
        fmap (map fst) (Lex.lex "X -> X+F") `shouldBe` (Right [TId "X", TArrow, TId "X", TSym  "+", TId "F", TSep])
    it "can scan a rule 2" $ do
        fmap (map fst) (Lex.lex "F -> F+F--F+F") `shouldBe`
            (Right [TId "F", TArrow, TId "F", TSym  "+", TId "F", TSym "-", TSym "-",
            TId "F", TSym "+", TId "F", TSep])
    it "can scan fern.lsys" $ do
        file <- readFile "./test/fixtures/fern.lsys"
        fmap (map fst) (Lex.lex file) `shouldBe`
            (Right [TAngle, TNumber 25.0, TSep, TAxiom, TId "X", TSep, TId "X",
            TArrow, TId "F", TSym "-", TSym "[", TSym  "[", TId "X", TSym "]", TSym "+", TId "X",
            TSym "]", TSym "+", TId "F", TSym "[", TSym "+", TId "F", TId "X", TSym  "]", TSym "-", TId "X", TSep, TId "F", TArrow, TId "F", TId "F", TSep])
    it "can scan kochcurve.lsys" $ do
        file <- readFile "./test/fixtures/kochcurve.lsys"
        fmap (map fst) (Lex.lex file) `shouldBe`
            (Right [TAngle, TNumber 90.0, TSep, TAxiom, TId "F", TSep, TId "F",
            TArrow, TId "F", TSym "+", TId "F", TSym  "-", TId "F", TSym "-", TId "F", TSym "+",
            TId "F", TSep])
    it "can scan rule with parameters: X(l) -> Y(l/2)-F(l/2)" $ do
        fmap (map fst) (Lex.lex "X(l) -> Y(l/2)-F(l/2)") `shouldBe`
            (Right [TId "X", TLParen, TId "l", TRParen, TArrow, TId "Y", TLParen, TId "l", TSym "/", TNumber 2, TRParen, TSym "-",
            TId "F", TLParen, TId "l", TSym "/", TNumber 2, TRParen,TSep])
