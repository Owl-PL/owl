module OwlCore.Syntax.PPrintSpec where

import Control.Monad.State.Lazy
import Test.Hspec
import Test.QuickCheck

import OwlCore.Syntax.AST as AST
import OwlCore.Syntax.PPrint

import OwlCore.Syntax.ASTTests

runMarkupExpr :: AST.Expr -> Markup String
runMarkupExpr expr = snd $ execState (markupExpr expr) (0, Nil)

pprintExpr :: AST.Expr -> String
pprintExpr = printMarkup . runMarkupExpr

spec :: Spec
spec = do
  describe "pprint" $ do
    it "a valid less-than expression" $ property $
      \e1 e2 -> pprintExpr (lessVExpr e1 e2) `shouldBe` ((pprintExpr e1) ++ " < " ++ (pprintExpr e2))
