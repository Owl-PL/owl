-- |Adds newlines to `AST.Expr` to make diffing easier.
module OwlCore.Test.DiffableAST where

import Data.Algorithm.Diff
import Data.Algorithm.DiffOutput
import Data.Algorithm.DiffContext
import Text.PrettyPrint hiding (Str)

import OwlCore.Syntax.AST
import OwlCore.Test.RandomAST

-- |A diffable string is essentially a string as a datatype.
--  This helps with performance by allowing us to not use list append.
data Diffable
  = Str String
  | Append Diffable Diffable

-- |Creates a diffable atomic expression. This only adds a newline
--  after a parathesized expression.
diffableAExpr :: AExpr -> Diffable
diffableAExpr (Paren e) = (Str "Paren (") `Append` ((diffableAST e) `Append` (Str ")\n"))
diffableAExpr ae = Str (show ae)

-- |Creates a diffable definition.  Adds a newline after the definition.
diffableDef :: Def -> Diffable
diffableDef (Def s e) = (Str "Def ") `Append` ((Str s) `Append` ((Str " (") `Append` ((diffableAST e) `Append` (Str "),\n"))))

-- |Creates a diffable alternative.  Adds a newline after the alternative.
diffableAlt :: Alt -> Diffable
diffableAlt (Alt i vs e) = (Str "Alt ") `Append` ((Str (show i)) `Append` ((Str " ") `Append` ((Str (show vs)) `Append` ((Str " (") `Append` ((diffableAST e) `Append` (Str "),\n"))))))

-- |Creates a diffable `Ast.Expr` by inserting newlines throughout the AST.
diffableAST :: Expr -> Diffable

diffableAST e@(App e1 e2)
  = (Str "App ((") `Append` ((diffableAST e1) `Append` ((Str ")\n(") `Append` ((diffableAExpr e2) `Append` (Str ")\n"))))
  
diffableAST e@(Binop op e1 e2)
  = (Str "Binop (") `Append` ((Str op)  `Append` ((Str " (") `Append` ((diffableAST e1) `Append` ((Str ")\n (") `Append` ((diffableAST e2) `Append` (Str ")\n"))))))

diffableAST e@(Let defs body)
  = (Str "Let [") `Append` ((foldl (\r d -> diffableDef(d) `Append` ((Str "\n") `Append` r)) (Str "") defs) `Append` ((Str "] (") `Append` ((diffableAST body) `Append` (Str ")\n"))))
  
diffableAST e@(LetRec defs body) 
  = (Str "LetRec [") `Append` ((foldl (\r d -> diffableDef(d) `Append` ((Str "\n") `Append` r)) (Str "") defs) `Append` ((Str "] (") `Append` ((diffableAST body) `Append` (Str ")\n"))))

diffableAST e@(Case e' alts)
  = (Str "Case (") `Append` ((diffableAST e') `Append` ((foldl (\r a -> diffableAlt(a) `Append` ((Str "\n") `Append` r)) (Str "") alts) `Append` (Str "\n")))

diffableAST e@(Fun binders body) = (Str "Fun ") `Append` ((Str . show $ binders) `Append` ((Str " (") `Append` ((diffableAST body) `Append` (Str ")\n"))))
diffableAST e@(Atomic ae) = (Str "Atomic (") `Append` ((diffableAExpr ae) `Append` (Str "\n"))

-- |Flattens a diffable into a string.
flattenDiff :: [Diffable] -> String
flattenDiff [] = ""
flattenDiff ((Str s) : ds) = s ++ flattenDiff ds
flattenDiff ((Append d1 d2) : ds) = flattenDiff (d1 : d2 : ds)

-- |Converts an expression into a `Diffable`, and then flattens to a `String`.
mkDiffableExpr :: Expr -> String
mkDiffableExpr e = flattenDiff [diffableAST e]

-- |Diffs two expressions as `String`'s.
diffExprStr :: String -> String -> String
diffExprStr e1 e2 = render $ prettyContextDiff (text "eIn") (text "eOut") text $ getContextDiff 2 (lines e1) (lines e2)

-- |Diffs two expression and outputs the diff to standard out.
diffExpr :: Expr -> Expr -> IO ()
diffExpr e1 e2 = putStrLn $ diffExprStr (mkDiffableExpr e1) (mkDiffableExpr e2)
