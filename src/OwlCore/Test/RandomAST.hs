{- |A framework for generating random OwlCore abstract syntax.
-}
module OwlCore.Test.RandomAST where

import Control.Monad
import Test.QuickCheck hiding (Fun)

import OwlCore.Syntax.AST hiding (Name)

-- * Variable Names #Names#

-- | The type of characters in variable names.
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

-- | A `Name` is a list of `NameChar`'s.
newtype Name = Name [NameChar]

-- | Generates a random lowercase `NameChar`.
--  This is used for choosing the first symbol in a variable name.
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
    

-- | Converts a `Name` to a `String`.
nameToString :: Name -> String
nameToString (Name ncs) = map toChar ncs
 where
   toChar :: NameChar -> Char
   toChar (NameChar x) = x
   
instance Show Name where
  show = nameToString

-- | Generates a random variable name (`Name`) as a `String` (using `nameToString`).
--  The name returned starts with a lowercase letter (using `genLAlpha`) and is
--  not a keyword of the language.
genName :: Gen String
genName = do
  n <- sized genName'
  let name = show n
  if name `elem` ["in", "of", "let", "fun", "letrec", "case"]
  then genName
  else return name
  where
   genName' :: Int -> Gen Name
   genName' n =
     do x <- genLAlpha
        vs <- vector $ n - 1
        return $ Name $ x : vs

-- | Generates a list of variable names using `genName` as a list of `String`'s.
genNames :: Gen [String]
genNames = do
  ns <- genNames'
  return $ ns
 where
   genNames' :: Gen [String]
   genNames' = sized $ \n -> listOf1 genName

-- * Natural Numbers #Nats#

-- | The type of a single digit that makes up a natural number.
newtype Digit = Digit Int

instance Arbitrary Digit where
  arbitrary = do
    n <- elements [0,1,2,3,4,5,6,7,8,9]
    return $ Digit n

-- | The type of natural numbers as a list of `Digit`'s.      
newtype Nat = Nat [Digit]

-- | Converts a `Nat` to an `Int`.
natToInt :: Nat -> Int
natToInt (Nat ns) = natToInt' ((length ns) - 1) 0 ns
  where
    natToInt' :: Int -> Int -> [Digit] -> Int
    natToInt' c i [] = i
    natToInt' c i ((Digit d) : ds) = natToInt' (c-1) (i + 10^c * d) ds
    
-- | Generates a random natural number as an `Int` using `natToInt`.
genNat :: Gen Int
genNat = do
  n <- genNat'
  return . natToInt $ n
  where
    genNat' = sized $ \n ->
      do ns <- vector $ if n > 0 then n else 1
         return (Nat (take 10 ns))


-- * Expressions #Expr#

-- | Chooses a random valid binary operator. 
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

-- | Generates a random atomic expression.
aExprGen :: Gen AExpr
aExprGen = sized aExprGen'
  where
    aExprGen' :: Int -> Gen AExpr
    aExprGen' n | n > 1 = liftM Paren expr
      where
        expr  = resize (n `div` 2) naExprGen
        
    aExprGen' _ = oneof [liftM Var genName,
                         liftM Num genNat,
                         liftM2 Pack genNat genNat]      


-- | Generates an expression where if it is not atomic will
--   be wrapped in parentheses.
parenExprGen :: Gen Expr
parenExprGen = do
  e <- exprGen
  return $ parenExpr e

-- |  Generates a non-atomic expression.
naExprGen :: Gen Expr
naExprGen = sized naExprGen'
  where
    naExprGen' :: Int -> Gen Expr
    naExprGen' n = oneof [liftM2 App pexpr aexpr,
                          liftM3 Binop genBinop pexpr pexpr,
                          liftM2 Let defs expr,
                          liftM2 LetRec defs expr,
                          liftM2 Case pexpr alts,
                          liftM2 Fun genNames pexpr]
      where
        pexpr  = resize (n `div` 2) parenExprGen
        naExpr = resize (n `div` 2) naExprGen
        expr   = resize (n `div` 2) exprGen
        aexpr  = resize (n `div` 2) aExprGen
        defs   = resize (n `div` 2) defsGen
        alts   = resize (n `div` 2) altsGen


-- | Generates a random minimally-fully parenthesized expression.
exprGen :: Gen Expr
exprGen = sized exprGen'
  where
    exprGen' :: Int -> Gen Expr
    exprGen' n | n > 1 = oneof [liftM (Atomic . Paren) naExpr,
                                liftM2 App pexpr aexpr,
                                liftM3 Binop genBinop pexpr pexpr,
                                liftM2 Let defs expr,
                                liftM2 LetRec defs expr,
                                liftM2 Case pexpr alts,
                                liftM2 Fun genNames pexpr]
      where
        pexpr  = resize (n `div` 2) parenExprGen
        naExpr = resize (n `div` 2) naExprGen
        expr   = resize (n `div` 2) exprGen
        aexpr  = resize (n `div` 2) aExprGen
        defs   = resize (n `div` 2) defsGen
        alts   = resize (n `div` 2) altsGen
                 
    exprGen' _ = oneof [liftM (Atomic . Var) genName,
                        liftM (Atomic . Num) genNat,
                        liftM2 (\n m -> Atomic (Pack n m)) genNat genNat]

-- | Generates a random definition.                 
defGen :: Gen Def
defGen = sized defGen'
 where
   defGen' :: Int -> Gen Def
   defGen' n = liftM2 Def genName expr
     where
       expr = resize (n `div` 2) parenExprGen

instance Arbitrary Def where
  arbitrary = defGen

-- | Generates a list of definitions.  
defsGen :: Gen [Def]
defsGen = sized defsGen'
  where
    defsGen' :: Int -> Gen [Def]
    defsGen' n = vector $ if n > 0 then n else 1

-- | Generates a random alternative.
altGen :: Gen Alt
altGen = sized altGen'
  where
    altGen' :: Int -> Gen Alt
    altGen' n = liftM3 Alt genNat genNames expr
      where
        expr = resize (n `div` 2) parenExprGen

instance Arbitrary Alt where
  arbitrary = altGen

-- | Generates a random list of alternatives using `altGen`.
altsGen :: Gen [Alt]
altsGen = sized altsGen'
  where
    altsGen' :: Int -> Gen [Alt]
    altsGen' n = vector $ if n > 0 then n else 1           

instance Arbitrary Expr where
  arbitrary = exprGen

-- | Generates random expression of size `n`.
genExpr :: Int -> IO Expr
genExpr n = generate $ (resize n exprGen)
