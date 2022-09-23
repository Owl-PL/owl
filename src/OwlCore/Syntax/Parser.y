{
-- | The Happy parser.
module OwlCore.Syntax.Parser (parseCoreExpr, parseCore) where
    
import qualified OwlCore.Syntax.Lexer as Lexer
import qualified OwlCore.Syntax.AST as AST  
import Utils.Error

import System.IO (hPutStrLn, stderr)    
    
}

%name parse
%tokentype { Lexer.Token }
%monad { ParseError } { bindExcept } { returnExcept }
%error { parseError }

%token
      fun    { Lexer.Fun _       }
      let    { Lexer.Let _       }
      letrec { Lexer.LetRec _    }
      case   { Lexer.Case _      }
      in     { Lexer.In _        }
      of     { Lexer.Of _        }
      Pack   { Lexer.Pack _      }
      num    { Lexer.Num _ $$    }
      var    { Lexer.Var _ $$    }
      altid  { Lexer.AltId _ $$  }
      '<'    { Lexer.Less _      }
      '<='   { Lexer.LEq _       }
      '>'    { Lexer.Gr _        }
      '>='   { Lexer.GrEq _      }
      '!='   { Lexer.Neq _       }
      '=='   { Lexer.Eq _        }
      '+'    { Lexer.Plus _      }
      '-'    { Lexer.Minus _     }
      '*'    { Lexer.Mult _      }
      '/'    { Lexer.Div _       }
      '&'    { Lexer.And _       }
      '|'    { Lexer.Or _        }
      '='    { Lexer.Equal _     }
      ';'    { Lexer.Semicolon _ }
      '{'    { Lexer.LBrace _    }
      '}'    { Lexer.RBrace _    }
      '('    { Lexer.LParen _    }
      ')'    { Lexer.RParen _    }
      ','    { Lexer.Comma _     }
      '->'   { Lexer.RArrow _    }       

%right in
%right of
%right '->'
%nonassoc '>' '>=' 
%nonassoc '<' '<='
%nonassoc '!=' '=='
%left '+' '-' 
%left '*' '/'
%left '&' '|'        
%%

Prog : Prog ';' SC { $3 : $1  }
     | Prog ';'    { $1       }
     | SC          { [$1]     }
     | {- empty -} { []       }

vars : var             { [$1]  }
     | vars var        { $2:$1 }
     | {- empty -}     { []    }

cvars : cvars ',' var { $3 : $1 }
      | var           { [$1]    }

SC : var vars '=' Expr { AST.SC $1 $2 $4 }

def : var '=' Expr { AST.Def $1 $3 }

defs : defs ';' def { $3 : $1 }
     | def          { [$1]    }

alt : altid vars '->' Expr  { AST.Alt $1 $2 $4 }

alts : alts ';' alt { $3 : $1 }
     | alt          { [$1]    }

binop : '<'   { AST.Less  }
      | '<='  { AST.LEq   }
      | '>'   { AST.Gr    }
      | '>='  { AST.GrEq  }
      | '+'   { AST.Plus  }
      | '-'   { AST.Minus }
      | '*'   { AST.Mult  }
      | '/'   { AST.Div   }
      | '!='  { AST.Neq   }
      | '=='  { AST.Eq    }
      | '&'   { AST.And   }
      | '|'   { AST.Or    }

Expr : Expr AExpr                  { AST.App $1 $2      }
     | Expr binop Expr             { AST.Binop $2 $1 $3 }
     | let defs in Expr            { AST.Let $2 $4      }
     | letrec defs in Expr         { AST.LetRec $2 $4   }
     | case Expr of alts           { AST.Case $2 $4     }
     | fun var '->' Expr           { AST.Fun [$2] $4    }
     | fun '(' cvars ')' '->' Expr { AST.Fun $3 $6      }
     | AExpr                       { AST.Atomic $1      }

AExpr : var                      { AST.Var $1     }
      | num                      { AST.Num $1     }
      | Pack '{' num ',' num '}' { AST.Pack $3 $5 }
      | '(' Expr ')'             { AST.Paren $2   }

{

type ParseError a = Except String a  

-- errorMessage :: [Lexer.Token] -> String
-- errorMessage [] = ""
-- errorMessage ((Fun (AlexPosn off line column)) : ts) = undefined
  
-- | Issues a parse error using the lexer stream.
parseError :: [Lexer.Token] -> ParseError a
parseError tks = Failed . show $ tks

-- | Parses an expression.    
parseCoreExpr :: String -> ParseError AST.Expr
parseCoreExpr e = do
  [ AST.SC _ _ e' ] <- parseCore sc
  return e'  
 where
    sc = "main = " ++ e
  
-- | Parses a program.
--   This is simply the composition of `Lexer.Lexer` and the Happy parser.
parseCore :: String -> ParseError AST.Prog
parseCore = parse  . Lexer.lexer

}
