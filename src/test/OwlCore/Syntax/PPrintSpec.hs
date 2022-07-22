module OwlCore.Syntax.PPrintSpec where

import Control.Monad.State.Lazy
import Test.Hspec
import Test.QuickCheck

import OwlCore.Syntax.AST as AST
import OwlCore.Syntax.Parser as Parser
import OwlCore.Syntax.PPrint

import OwlCore.Syntax.ASTTests

prop e = (Parser.parseCoreExpr (pprintExpr e)) `shouldBe` e

spec :: Spec
spec = do
  describe "pprintExpr:" $ do
    it "Pretty printing and then parsing should be the identity" $ property prop
      
