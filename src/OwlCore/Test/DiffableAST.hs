-- | Create diffs of expressions for analysis during verification.
module OwlCore.Test.DiffableAST (diffExpr) where

import Data.Algorithm.Diff
import Data.Algorithm.DiffOutput
import Data.Algorithm.DiffContext
import Text.PrettyPrint hiding (Str)

import OwlCore.Syntax.AST
import OwlCore.Test.RandomAST
import Utils.Snoc

-- | Breaks up the input string into substrings of size `l`.
--   To be as efficent as possible we use snoc lists as an
--   intermediate representation.
diffable :: Int -> String -> String
diffable l s = foldr1 (++) . unsnoc $ smap unsnoc (sdiffable s)
  where
    sdiffable :: String -> SList (SList Char)
    sdiffable = aux 0 Empty Empty 
      where
        aux :: Int -> SList Char -> SList (SList Char) -> String -> SList (SList Char)
        aux _ s Empty [] = Empty `Snoc` s
        aux _ s acc [] = Snoc acc s
        aux count s acc (x:xs) | count >= l && x == ' ' = aux 0 Empty (Snoc acc (Snoc s '\n')) xs
                               | otherwise = aux (count+1) (Snoc s x) acc xs

-- | Diffs two expressions as `String`'s.
diffExprStr :: String -> String -> String
diffExprStr eIn eOut = render $ prettyContextDiff (text "eIn") (text "eOut") text $ getContextDiff 2 (lines eIn) (lines eOut)

-- | Diffs two expression and outputs the diff to standard out.
diffExpr :: Expr -> Expr -> IO ()
diffExpr e1 e2 = putStrLn $ diffExprStr ((diffable 100) . show $ e1) ((diffable 100) . show $ e2)

