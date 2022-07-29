{
-- | Lexical analysis.
module OwlCore.Syntax.Lexer (Token(..), lexer) where

import Control.Monad.State
import Control.Monad.Except
import Data.Word

}

%wrapper "posn"

$digit   = 0-9
$lalpha  = a-z
$ualpha  = A-Z
$varch   = [a-z A-Z 0-9 \_]
$arithop = [\+ \- \* \/]
$boolop  = [\&\|]

@var     = $lalpha $varch*

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
  "<"$digit+">"              {tok (\p s -> AltId p (read . init . tail $ s))}
  ">"                        {tok (\p s -> Gr p)}
  ">="                       {tok (\p s -> GrEq p)}
  "<"                        {tok (\p s -> Less p)}
  "<="                       {tok (\p s -> LEq p)}
  "!="                       {tok (\p s -> Neq p)}
  "=="                       {tok (\p s -> Eq p)}
  "+"                        {tok (\p s -> Plus p)}
  "-"                        {tok (\p s -> Minus p)}
  "*"                        {tok (\p s -> Mult p)}
  "/"                        {tok (\p s -> Div p)}
  "&"                        {tok (\p s -> And p)}
  "|"                        {tok (\p s -> Or p)}
  @var                       {tok (\p s -> Var p s)}
  "="                        {tok (\p s -> Equal p) }
  "{"                        {tok (\p s -> LBrace p)}
  "}"                        {tok (\p s -> RBrace p)}
  ")"                        {tok (\p s -> RParen p)}
  "("                        {tok (\p s -> LParen p)}
  ","                        {tok (\p s -> Comma p)}
  "->"                       {tok (\p s -> RArrow p)}
  ";"                        {tok (\p s -> Semicolon p)}
  
{

-- Wraps `token` to make it easier to get the position and the input string.
tok f p s = f p s

-- | The lexical tokens of OwlCore.  They are all pretty self explanetory.  Each
--   constructor holds onto a `AlexPosn` which is the position of the token in the
--   input stream. We will use this for error reporting.
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
  | AltId      AlexPosn Int
  | Equal      AlexPosn
  | Less       AlexPosn
  | LEq        AlexPosn
  | Gr         AlexPosn
  | GrEq       AlexPosn
  | Neq        AlexPosn
  | Eq         AlexPosn
  | Plus       AlexPosn
  | Minus      AlexPosn
  | Mult       AlexPosn
  | Div        AlexPosn
  | And        AlexPosn
  | Or         AlexPosn
  | Semicolon  AlexPosn
  | LBrace     AlexPosn
  | RBrace     AlexPosn
  | LParen     AlexPosn
  | RParen     AlexPosn
  | Comma      AlexPosn
  | RArrow     AlexPosn  
  deriving Show

-- | The lexer: translates a string into tokens.
lexer :: String -> [Token]
lexer = alexScanTokens

}
