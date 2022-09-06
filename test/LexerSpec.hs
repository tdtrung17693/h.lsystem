module LexerSpec(lexerSpec) where
import Test.Hspec ( it, shouldBe, SpecWith )
import qualified Lex as L

lexerSpec :: SpecWith ()
lexerSpec =  do
    it "can scan axiom" $ do
        fmap (map fst) (L.lex "axiom X") `shouldBe` (Right [L.TAxiom, L.TId "X", L.TSep])
    it "can scan angle" $ do
        fmap (map fst) (L.lex "angle 90") `shouldBe` (Right [L.TAngle, L.TNumber 90.0, L.TSep])
    it "can scan both axiom and angle" $ do
        fmap (map fst) (L.lex "axiom X\nangle 90" )`shouldBe` (Right [L.TAxiom, L.TId "X", L.TSep, L.TAngle, L.TNumber 90, L.TSep])
    it "can scan a rule" $ do
        fmap (map fst) (L.lex "X -> X+F") `shouldBe` (Right [L.TId "X", L.TArrow, L.TId "X", L.TSym  "+", L.TSym "F", L.TSep])
    it "can scan a rule 2" $ do
        fmap (map fst) (L.lex "F -> F+F--F+F") `shouldBe`
            (Right [L.TSym "F", L.TArrow, L.TSym "F", L.TSym  "+", L.TSym "F", L.TSym "-", L.TSym "-",
            L.TSym "F", L.TSym "+", L.TSym "F", L.TSep])
    it "can scan fern.lsys" $ do
        file <- readFile "./test/fixtures/fern.lsys"
        fmap (map fst) (L.lex file) `shouldBe`
            (Right [L.TAngle, L.TNumber 25.0, L.TSep, L.TAxiom, L.TId "X", L.TSep, L.TId "X",
            L.TArrow, L.TSym "F", L.TSym "-", L.TSym "[", L.TSym  "[", L.TId "X", L.TSym "]", L.TSym "+", L.TId "X",
            L.TSym "]", L.TSym "+", L.TSym "F", L.TSym "[", L.TSym "+", L.TSym "F", L.TId "X", L.TSym  "]", L.TSym "-", L.TId "X", L.TSep, L.TSym "F", L.TArrow, L.TSym "F", L.TSym  "F", L.TSep])
    it "can scan kochcurve.lsys" $ do
        file <- readFile "./test/fixtures/kochcurve.lsys"
        fmap (map fst) (L.lex file) `shouldBe`
            (Right [L.TAngle, L.TNumber 90.0, L.TSep, L.TAxiom, L.TSym "F", L.TSep, L.TSym "F",
            L.TArrow, L.TSym "F", L.TSym "+", L.TSym "F", L.TSym  "-", L.TSym "F", L.TSym "-", L.TSym "F", L.TSym "+",
            L.TSym "F", L.TSep])
    