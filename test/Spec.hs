import Test.Hspec
import Assembly.Core
import Assembly.ControlFlowSpec -- Import the new test module
import Control.Exception (evaluate)
import Assembly.ListSpec -- Import the new test module

main :: IO ()
main = hspec $ do
  describe "evalLabelExpr" $ do
    it "handles LabelAdd and LabelSub correctly" $ do
      let expr1 = LabelAdd (LabelRef "test") 2
      evaluate (evalLabelExpr expr1) `shouldThrow` errorCall "Compile-time error: Cannot get value of unresolved label 'test' within expression"

      let expr2 = LabelSub (LabelRef "test") 1
      evaluate (evalLabelExpr expr2) `shouldThrow` errorCall "Compile-time error: Cannot get value of unresolved label 'test' within expression"

    it "handles LabelParen correctly" $ do
      let expr = LabelParen (LabelRef "test")
      evaluate (evalLabelExpr expr) `shouldThrow` errorCall "Compile-time error: Cannot get value of unresolved label 'test' within expression"

    it "handles nested expressions correctly" $ do
      let expr = LabelParen (LabelAdd (LabelRef "test") 2)
      evaluate (evalLabelExpr expr) `shouldThrow` errorCall "Compile-time error: Cannot get value of unresolved label 'test' within expression"

      let expr2 = LabelAdd (LabelParen (LabelRef "test")) 2
      evaluate (evalLabelExpr expr2) `shouldThrow` errorCall "Compile-time error: Cannot get value of unresolved label 'test' within expression"

  Assembly.ControlFlowSpec.spec -- Include the tests from ControlFlowSpec
  Assembly.ListSpec.spec -- Include the tests from ListSpec
