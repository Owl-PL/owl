{
-- The Alex lexer.
module OwlCore.Syntax.Lexer (Token(..), alexScanTokens) where

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
  ">"                        {tok (\p s -> Binop p ">")}
  ">="                       {tok (\p s -> Binop p ">=")}
  "<"                        {tok (\p s -> Binop p "<")}
  "<="                       {tok (\p s -> Binop p "<=")}
  "!="                       {tok (\p s -> Binop p "!=")}
  "=="                       {tok (\p s -> Binop p "==")}
  [$arithop $boolop]         {tok (\p s -> Binop p s)}
  @var                       {tok (\p s -> Var p s)}
  "="                        {tok (\p s -> Equal p) }
  "{"                        {tok (\p s -> LBrace p)}
  "}"                        {tok (\p s -> RBrace p)}
  ")"                        {tok (\p s -> RParen p)}
  "("                        {tok (\p s -> LParen p)}
  ","                        {tok (\p s -> Comma p)}
  \\                         {tok (\p s -> Slash p)}
  "->"                       {tok (\p s -> RArrow p)}
  "."                        {tok (\p s -> Period p)}
  ";"                        {tok (\p s -> Semicolon p)}
  
{

-- Wraps `token` to make it easier to get the position and the input string.
tok f p s = f p s

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
  | AltId      AlexPosn Int
  | Binop      AlexPosn String
  | Equal      AlexPosn
  | Semicolon  AlexPosn
  | LBrace     AlexPosn
  | RBrace     AlexPosn
  | LParen     AlexPosn
  | RParen     AlexPosn
  | Comma      AlexPosn
  | Slash      AlexPosn
  | RArrow     AlexPosn
  | Period     AlexPosn
  deriving Show
}
