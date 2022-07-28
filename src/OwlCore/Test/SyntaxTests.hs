-- | The QuickCheck Specification for syntatic operations.
module OwlCore.Test.SyntaxTests where

import Test.Hspec

import OwlCore.Syntax.AST
import OwlCore.Syntax.Parser
import OwlCore.Syntax.PPrint

-- | The property specifying that composing the pretty printer with
--   the parser yeilds the identity function.
parserInversePretty :: Expr -> Expectation
parserInversePretty e =  (parseCoreExpr (pprintExpr e)) `shouldBe` e

