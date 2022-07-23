module OwlCore.Syntax.PPrintSpec where

import Test.Hspec
import Test.QuickCheck

import OwlCore.Test.RandomAST
import OwlCore.Test.SyntaxTests

{-| The hspec specification for `parserInversePretty`.
-}
spec :: Spec
spec = do
  describe "pprintExpr:" $ do
    it "Parsing is the inverse of pretty printing." $ property parserInversePretty
      
