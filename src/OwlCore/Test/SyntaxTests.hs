-- | The QuickCheck Specification for syntatic operations.
module OwlCore.Test.SyntaxTests where

import Test.Hspec
import Test.QuickCheck hiding (Fun)

import OwlCore.Syntax.AST
import OwlCore.Syntax.Parser
import OwlCore.Syntax.PPrint
import OwlCore.Test.RandomAST

-- | The property specifying that composing the pretty printer with
--   the parser yeilds the identity function.
parserInversePretty :: Expr -> Property
parserInversePretty e =  label ("length of AST: "++ show (length (show e))) $
  (parseCoreExpr (pprintExpr e)) `shouldBe` e

