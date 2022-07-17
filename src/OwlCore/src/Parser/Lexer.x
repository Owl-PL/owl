{
-- The Alex lexer.
module Parser.Lexer where
}

%wrapper "posn"

$digit = 0-9
$lalpha = a-z
$ualpha = A-Z
$defsym = [\=\;\{\}\(\)\,\.]
$varch = [a-zA-Z0-9\_]
$arithop = [\+\-\*\/]
$relop = [\< \<\= \> \>\= \~\= \=\=]
$boolop = [\&\|]

tokens :-
  $white+ ;
  "--".*  ;
  let                       {tok (\p s -> Let p)}
  letrec                    {tok (\p s -> LetRec p)}
  case                      {tok (\p s -> Case p)}
  in                        {tok (\p s -> In p)}
  of                        {tok (\p s -> Of p)}
  Pack                      {tok (\p s -> Pack p)}
  $digit+                   {tok (\p s -> Num p (read s))}
  $lalpha $varch*           {tok (\p s -> Var p s)}
  "<"$digit+">"              {tok (\p s -> AltId p s)}
  [$arithop $relop $boolop] {tok (\p s -> Binop p (head s))}
  $defsym                   {tok (\p s -> Sym p (head s))}

{

tok f p s = f p s

-- The tokens:
data Token
  = Let        AlexPosn
  | LetRec     AlexPosn
  | Case       AlexPosn
  | In         AlexPosn
  | Of         AlexPosn
  | Pack       AlexPosn
  | Num        AlexPosn Int    
  | Var        AlexPosn String
  | AltId      AlexPosn String
  | Binop      AlexPosn Char
  | Sym        AlexPosn Char
  deriving (Eq,Show)

token_posn (Let p)      = p
token_posn (LetRec p)   = p
token_posn (Case p)     = p
token_posn (In p)       = p
token_posn (Of p)       = p
token_posn (Pack p)     = p
token_posn (Num p _)    = p
token_posn (Var p _)    = p
token_posn (AltId p _)  = p
token_posn (Binop p _)  = p
token_posn (Sym p _)    = p

runLexer = do
  s <- getContents
  print (alexScanTokens s)
}
