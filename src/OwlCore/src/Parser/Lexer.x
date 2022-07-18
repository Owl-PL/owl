{
-- The Alex lexer.
module Parser.Lexer (Token(..),Alex,lexerCore, alexEOF) where

import Control.Monad.State
import Control.Monad.Except
import Data.Word

}

%wrapper "monad"

$digit = 0-9
$lalpha = a-z
$ualpha = A-Z
$defsym = [\= \; \{ \} \( \) \, \.]
$varch = [a-zA-Z0-9\_]
$arithop = [\+\-\*\/]
$relop = [\< \<\= \> \>\= !\= \=\=]
$boolop = [\&\|]

tokens :-
  $white+ ;
  "--".*  ;
  fun                        {tok (\p s -> Fun p)}
  let                        {tok (\p s -> Let p)}
  letrec                     {tok (\p s -> LetRec p)}
  case                       {tok (\p s -> Case p)}
  in                         {tok (\p s -> In p)}
  of                         {tok (\p s -> Of p)}
  Pack                       {tok (\p s -> Pack p)}
  $digit+                    {tok (\p s -> Num p (read s))}
  $lalpha $varch*            {tok (\p s -> Var p s)}
  "<"$digit+">"              {tok (\p s -> AltId p s)}
  [$arithop $relop $boolop]  {tok (\p s -> Binop p s)}
  $defsym                    {tok (\p s -> Sym p s)}

{

-- Wraps `token` to make it easier to get the position and the input string.
tok :: (AlexPosn -> String -> Token) -> AlexAction Token
tok f (pos,prev,rest,str) = token (\input len -> f pos str) (pos,prev,rest,str)

-- The tokens:
data Token
  = Fun        AlexPosn
  | Let        AlexPosn
  | LetRec     AlexPosn
  | Case       AlexPosn
  | In         AlexPosn
  | Of         AlexPosn
  | Pack       AlexPosn
  | Num        AlexPosn Int    
  | Var        AlexPosn String
  | AltId      AlexPosn String
  | Binop      AlexPosn String
  | Sym        AlexPosn String
  | EOF        
  deriving (Eq,Show)

alexEOF :: Alex Token
alexEOF = return EOF

lexerCore :: (Token -> Alex result) -> Alex result
lexerCore cont = do
    token <- alexMonadScan
    cont token

}
