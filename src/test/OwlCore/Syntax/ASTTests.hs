module OwlCore.Syntax.ASTTests where

import Control.Monad
import Test.QuickCheck hiding (Fun)

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

nameToString :: Name -> String
nameToString (Name ncs) = map toChar ncs
 where
   toChar :: NameChar -> Char
   toChar (NameChar x) = x
   
instance Show Name where
  show = nameToString

genName' :: Int -> Gen Name
genName' n = do vs <- vector $ if n > 0 then n else 1
                return $ Name vs

instance Arbitrary Name where
  arbitrary = sized genName'

genName :: Gen String
genName = do
  n <- sized genName'
  return . show $ n      

genNames :: Gen [String]
genNames = do
  ns <- genNames'
  return $ map show ns
 where
   genNames' :: Gen [Name]
   genNames' = sized $ \n -> vector $ if n > 0 then n else 1

newtype Digit = Digit Int

instance Arbitrary Digit where
  arbitrary = do
    n <- elements [0,1,2,3,4,5,6,7,8,9]
    return $ Digit n
       
newtype Nat = Nat [Digit]

genNat' = sized $ \n ->
  do ns <- vector $ if n > 0 then n else 1
     return (Nat ns)

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
                  return "\\",
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

exprGen :: Gen Expr
exprGen = sized exprGen'

exprGen' :: Int -> Gen Expr
exprGen' n | n > 1 = oneof [liftM (Atomic . Paren) expr,
                            liftM2 App expr aexpr,
                            liftM3 Binop genBinop expr expr,
                            liftM2 Let defs expr,
                            liftM2 LetRec defs expr,
                            liftM2 Case expr alts,
                            liftM2 Fun genNames expr]
  where
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
    expr = resize (n `div` 2) exprGen

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
    expr = resize (n `div` 2) exprGen

instance Arbitrary Alt where
  arbitrary = altGen

altsGen :: Gen [Alt]
altsGen = sized altsGen'

altsGen' :: Int -> Gen [Alt]
altsGen' n = vector $ if n > 0 then n else 1           
instance Arbitrary Expr where
  arbitrary = exprGen

lessVExpr e1 e2 = Binop "<" e1 e2

