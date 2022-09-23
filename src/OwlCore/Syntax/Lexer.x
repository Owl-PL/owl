{
-- | Lexical analysis.
module OwlCore.Syntax.Lexer where

import Control.Monad.State
import Control.Monad.Except
import Data.Word
import Numeric (getDec)

-- | The lexical tokens of OwlCore.  They are all pretty self explanetory.  Each
--   constructor holds onto a `AlexPosn` which is the position of the token in the
--   input stream. We will use this for error reporting.
data TokenType
  = Num   Int    
  | Var   String
  | AltId Int
  | Fun   
  | Let   
  | LetRec
  | Case  
  | In    
  | Of    
  | Pack    
  | Equal 
  | Less  
  | LEq   
  | Gr    
  | GrEq  
  | Neq   
  | Eq    
  | Plus  
  | Minus 
  | Mult  
  | Div   
  | And   
  | Or    
  | Semicolon
  | LBrace   
  | RBrace   
  | LParen   
  | RParen   
  | Comma    
  | RArrow   
  | EOF       
  deriving Show

data Token = Token AlexPosn TokenType
  deriving Show

tokenPosn :: Token -> AlexPosn
tokenPosn (Token p _) = p

tokenType :: Token -> TokenType
tokenType (Token _ tok) = tok

alexEOF :: Alex Token
alexEOF = return EOF

type Action = AlexInput -> Int -> Alex Token

}

%wrapper "monadUserState"

$digit   = 0-9
$lalpha  = a-z
$ualpha  = A-Z
$varch   = [a-z A-Z 0-9 \_]

@var     = $lalpha $varch*

state :-

<0>  $white+                    ;
<0>  "--".*                     ;
<0>  fun                        { mkToken Fun       }
<0>  let                        { mkToken Let       }
<0>  letrec                     { mkToken LetRec    }
<0>  case                       { mkToken Case      }
<0>  in                         { mkToken In        }
<0>  of                         { mkToken Of        }
<0>  Pack                       { mkToken Pack      }
<0>  ">"                        { mkToken Gr        }
<0>  ">="                       { mkToken GrEq      }
<0>  "<"                        { mkToken Less      }
<0>  "<="                       { mkToken LEq       }
<0>  "!="                       { mkToken Neq       }
<0>  "=="                       { mkToken Eq        }
<0>  "+"                        { mkToken Plus      }
<0>  "-"                        { mkToken Minus     }
<0>  "*"                        { mkToken Mult      }
<0>  "/"                        { mkToken Div       }
<0>  "&"                        { mkToken And       }
<0>  "|"                        { mkToken Or        }
<0>  "="                        { mkToken Equal     }
<0>  "{"                        { mkToken LBrace    }
<0>  "}"                        { mkToken RBrace    }
<0>  ")"                        { mkToken RParen    }
<0>  "("                        { mkToken LParen    }
<0>  ","                        { mkToken Comma     }
<0>  "->"                       { mkToken RArrow    }
<0>  ";"                        { mkToken Semicolon }
<0>  @var                       { mkTokenVar        }
<0>  $digit+                    { mkTokenNum        }  
<0>  "<"$digit+">"              { mkTokenAltId      }

{

-- * Actions

mkToken :: TokenType -> Action
mkToken tok (posn, _, _, input) len = return $ (Token posn tok)

mkTokenNum :: Action
mkTokenNum (pos, _, _, input) len = if (num == 1)
                                    then return $ Token p (Num (fst (head num)))
                                    else lexerError "Invalid number"
  where
    numStr = take len input
    num = getDec numStr

mkTokenVar :: Action
mkTokenVar (pos, _, _, input) len = return $ Token pos (Var s)
  where
    s = take len input

mkTokenAltId :: Action
mkTokenAltId (pos, _, _, input) len = if (num == 1)
                                    then return $ Token p (AltId (fst (head num)))
                                    else lexerError "Invalid alternative"
  where
    numStr = take len input
    num = getDec numStr 

lexerError :: String -> Alex result
lexerError = do
  (p, _, _, input) <- getAlexInput
  alexError input

-- * States

state_initial :: Int
state_initial = 0

-- | The lexer: translates a string into tokens.
--lexer :: Alex Token
--lexer = alexMonadScan

-- * The user state

data AlexUserState = AlexUserState {
    -- used by the lexer phase
    -- Add fields as needed
                     
    -- used by the parser phase
    parserCurrentToken :: Maybe Token
  , parserPos          :: Maybe AlexPosn
}
                   
alexInitUserState :: AlexUserState
alexInitUserState = AlexUserState {                       
    parserCurrentToken = Nothing
  , parserPos          = Nothing
}

getParserCurrentToken :: Alex Token
getParserCurrentToken = Alex getAux
  where
    getAux s@AlexState{alex_ust=ust} = Right (s, parserCurrentToken ust)

setParserCurrentToken :: Token -> Alex ()
setParserCurrentToken tok = Alex setAux
  where
    setAux s = Right (s { alex_ust = (alex_ust s) {parserCurrentToken = tok} }, ())

getParserPos :: Alex (Maybe AlexPosn)
getParserPos = Alex getAux
  where
    getAux s@AlexState{ alex_ust = ust } = Right (s, parserPos ust)

setParserPos :: Maybe AlexPosn -> Alex ()
setParserPos mposn = Alex setAux
  where
    setAux s = Right (s { alex_ust = (alex_ust s) {parserPos = ss }}, ())

line_number :: Maybe AlexPosn -> (Int, Int)
line_number Nothing                   = (0, 0)
line_number (Just (AlexPn _ lig col)) = (lig, col)



}
