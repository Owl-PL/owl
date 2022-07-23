module OwlCore.Syntax.ASTTests where

import Control.Monad
import Test.QuickCheck hiding (Fun)
import Data.Algorithm.Diff
import Data.Algorithm.DiffOutput
import Data.Algorithm.DiffContext
import Text.PrettyPrint hiding (Str)

import OwlCore.Syntax.AST

newtype NameChar = NameChar Char

instance Arbitrary NameChar where
  arbitrary = 
    elements [NameChar 'a',
              NameChar 'b',
              NameChar 'c',
              NameChar 'd',
              NameChar 'e',
              NameChar 'f',
              NameChar 'g',
              NameChar 'h',
              NameChar 'i',
              NameChar 'j',
              NameChar 'k',
              NameChar 'l',
              NameChar 'm',
              NameChar 'n',
              NameChar 'o',
              NameChar 'p',
              NameChar 'q',
              NameChar 'r',
              NameChar 's',
              NameChar 't',
              NameChar 'u',
              NameChar 'v',
              NameChar 'w',
              NameChar 'x',
              NameChar 'y',
              NameChar 'z',
              NameChar 'A',
              NameChar 'B',
              NameChar 'C',
              NameChar 'D',
              NameChar 'E',
              NameChar 'F',
              NameChar 'G',
              NameChar 'H',
              NameChar 'I',
              NameChar 'J',
              NameChar 'K',
              NameChar 'L',
              NameChar 'M',
              NameChar 'N',
              NameChar 'O',
              NameChar 'P',
              NameChar 'Q',
              NameChar 'R',
              NameChar 'S',
              NameChar 'T',
              NameChar 'U',
              NameChar 'V',
              NameChar 'W',
              NameChar 'X',
              NameChar 'Y',
              NameChar 'Z',
              NameChar '0',
              NameChar '1',
              NameChar '2',
              NameChar '3',
              NameChar '4',
              NameChar '5',
              NameChar '6',
              NameChar '7',
              NameChar '8',
              NameChar '9',
              NameChar '_']

newtype Name = Name [NameChar]

genLAlpha :: Gen NameChar
genLAlpha = elements [NameChar 'a',
              NameChar 'b',
              NameChar 'c',
              NameChar 'd',
              NameChar 'e',
              NameChar 'f',
              NameChar 'g',
              NameChar 'h',
              NameChar 'i',
              NameChar 'j',
              NameChar 'k',
              NameChar 'l',
              NameChar 'm',
              NameChar 'n',
              NameChar 'o',
              NameChar 'p',
              NameChar 'q',
              NameChar 'r',
              NameChar 's',
              NameChar 't',
              NameChar 'u',
              NameChar 'v',
              NameChar 'w',
              NameChar 'x',
              NameChar 'y',
              NameChar 'z']
    

nameToString :: Name -> String
nameToString (Name ncs) = map toChar ncs
 where
   toChar :: NameChar -> Char
   toChar (NameChar x) = x
   
instance Show Name where
  show = nameToString

genName' :: Int -> Gen Name
genName' n =
   do x <- genLAlpha
      vs <- vector $ n - 1
      return $ Name $ x : vs

genName :: Gen String
genName = do
  n <- sized genName'
  let name = show n
  if name `elem` ["in", "of", "let", "fun", "letrec", "case"]
  then genName
  else return name

genNames :: Gen [String]
genNames = do
  ns <- genNames'
  return $ ns
 where
   genNames' :: Gen [String]
   genNames' = sized $ \n -> listOf1 genName

newtype Digit = Digit Int

instance Arbitrary Digit where
  arbitrary = do
    n <- elements [0,1,2,3,4,5,6,7,8,9]
    return $ Digit n
       
newtype Nat = Nat [Digit]

genNat' = sized $ \n ->
  do ns <- vector $ if n > 0 then n else 1
     return (Nat (take 10 ns))

instance Arbitrary Nat where
  arbitrary = genNat'  

natToInt :: Nat -> Int
natToInt (Nat ns) = natToInt' (length ns) 0 ns
  where
    natToInt' :: Int -> Int -> [Digit] -> Int
    natToInt' c i [] = i
    natToInt' c i ((Digit d) : ds) = natToInt' (c-1) (10^c * d + i) ds 

genNat :: Gen Int
genNat = do
  n <- genNat'
  return . natToInt $ n


genBinop :: Gen String
genBinop = oneof [return ">",
                  return "<" ,
                  return "<=",
                  return ">=",
                  return "+",
                  return "-",
                  return "/",
                  return "*",
                  return "&",
                  return "|",
                  return "==",
                  return "!="]

aExprGen :: Gen AExpr
aExprGen = sized aExprGen'

aExprGen' :: Int -> Gen AExpr
aExprGen' n | n > 1 = liftM Paren (exprGen' (n `div` 2))
aExprGen' _ = oneof [liftM Var genName,
                     liftM Num genNat,
                     liftM2 Pack genNat genNat]

parenExprGen :: Gen Expr
parenExprGen = do
  e <- exprGen
  return $ parenExpr e

exprGen :: Gen Expr
exprGen = sized exprGen'

exprGen' :: Int -> Gen Expr
exprGen' n | n > 1 = oneof [liftM (Atomic . Paren) expr,
                            liftM2 App pexpr aexpr,
                            liftM3 Binop genBinop pexpr pexpr,
                            liftM2 Let defs expr,
                            liftM2 LetRec defs expr,
                            liftM2 Case pexpr alts,
                            liftM2 Fun genNames pexpr]
  where
    pexpr  = resize (n `div` 2) parenExprGen
    expr  = resize (n `div` 2) exprGen
    aexpr = resize (n `div` 2) aExprGen
    defs  = resize (n `div` 2) defsGen
    alts  = resize (n `div` 2) altsGen
                                
exprGen' _ = oneof [liftM (Atomic . Var) genName,
                    liftM (Atomic . Num) genNat,
                    liftM2 (\n m -> Atomic (Pack n m)) genNat genNat]
             
defGen :: Gen Def
defGen = sized defGen'

defGen' :: Int -> Gen Def
defGen' n = liftM2 Def genName expr
  where
    expr = resize (n `div` 2) parenExprGen

instance Arbitrary Def where
  arbitrary = defGen
  
defsGen :: Gen [Def]
defsGen = sized defsGen'

defsGen' :: Int -> Gen [Def]
defsGen' n = vector $ if n > 0 then n else 1

altGen :: Gen Alt
altGen = sized altGen'

altGen' :: Int -> Gen Alt
altGen' n = liftM3 Alt genNat genNames expr
  where
    expr = resize (n `div` 2) parenExprGen

instance Arbitrary Alt where
  arbitrary = altGen

altsGen :: Gen [Alt]
altsGen = sized altsGen'

altsGen' :: Int -> Gen [Alt]
altsGen' n = vector $ if n > 0 then n else 1           

instance Arbitrary Expr where
  arbitrary = exprGen

-- AST Diff library.

data Diffable
  = Str String
  | Append Diffable Diffable

diffableAExpr :: AExpr -> Diffable
diffableAExpr (Paren e) = (Str "Paren (") `Append` ((diffableAST e) `Append` (Str ")\n"))
diffableAExpr ae = Str (show ae)

diffableDef :: Def -> Diffable
diffableDef (Def s e) = (Str "Def ") `Append` ((Str s) `Append` ((Str " (") `Append` ((diffableAST e) `Append` (Str "),\n"))))

diffableAlt :: Alt -> Diffable
diffableAlt (Alt i vs e) = (Str "Alt ") `Append` ((Str (show i)) `Append` ((Str " ") `Append` ((Str (show vs)) `Append` ((Str " (") `Append` ((diffableAST e) `Append` (Str "),\n"))))))

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

flattenDiff :: [Diffable] -> String
flattenDiff [] = ""
flattenDiff ((Str s) : ds) = s ++ flattenDiff ds
flattenDiff ((Append d1 d2) : ds) = flattenDiff (d1 : d2 : ds)

mkDiffableExpr :: Expr -> String
mkDiffableExpr e = flattenDiff [diffableAST e]

diffExprStr :: String -> String -> String
diffExprStr e1 e2 = render $ prettyContextDiff (text "eIn") (text "eOut") text $ getContextDiff 2 (lines e1) (lines e2)

diffExpr :: Expr -> Expr -> IO ()
diffExpr e1 e2 = putStrLn $ diffExprStr (mkDiffableExpr e1) (mkDiffableExpr e2)

eout = LetRec [Def "vUfzT6qIW" (Atomic (Paren (LetRec [Def "d" (Atomic (Var "z"))] (Atomic (Var "o"))))),Def "i4WSrgfn1" (Atomic (Paren (LetRec [Def "xM" (Atomic (Num 0)),Def "m0" (Atomic (Var "e"))] (LetRec [Def "i" (Atomic (Var "j"))] (Atomic (Var "q")))))),Def "guzLKMu4t" (Atomic (Paren (Fun ["qcvi","eEAz","fZB1","gWy5"] (Atomic (Paren (Binop ">" (Atomic (Num 10)) (Atomic (Var "j")))))))),Def "rQmGucmzQ" (Atomic (Paren (Fun ["kq0F","hh5K","jYsH","wvmU"] (Atomic (Paren (Let [Def "t" (Atomic (Pack 50 60))] (Atomic (Var "t")))))))),Def "n18Wl9lgs" (Atomic (Paren (Fun ["fnAi","n7WB","eZdf","gfSo"] (Atomic (Paren (Fun ["c1","e1"] (Atomic (Var "a")))))))),Def "v_ucDXzcD" (Atomic (Paren (Let [Def "i5" (Atomic (Var "w")),Def "k7" (Atomic (Var "o"))] (LetRec [Def "k" (Atomic (Pack 40 80))] (Atomic (Var "s")))))),Def "qEkcifqRU" (Atomic (Paren (Binop "==" (Atomic (Paren (Atomic (Var "g")))) (Atomic (Paren (Binop "==" (Atomic (Pack 0 70)) (Atomic (Pack 60 0)))))))),Def "r8nen0fJN" (Atomic (Paren (Let [Def "m3" (Atomic (Num 40)),Def "yO" (Atomic (Num 0))] (LetRec [Def "g" (Atomic (Var "u"))] (Atomic (Var "a")))))),Def "dgIa7Ipsu" (Atomic (Paren (Case (Atomic (Paren (Fun ["tN","qM"] (Atomic (Num 60))))) [Alt 110 ["in","m5"] (Atomic (Pack 20 60)),Alt 490 ["an","bf"] (Atomic (Var "y"))])))] (Fun ["mk2nvMDiA","r0GdWkinv","jteBu_oZc","d78s45B48","ejdwlFERo","pxXBMy3xj","om4pJ1hAx","rzLR6ZeaH","btrTTLroP"] (Atomic (Paren (Fun ["njpx","bAbA","pwnC","zcvy"] (Atomic (Paren (Binop "<=" (Atomic (Pack 30 40)) (Atomic (Num 0)))))))))
