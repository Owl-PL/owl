{

module Parser.Grammar where

import qualified Parser.Lexer as Lexer

}

%name parseCore
%tokentype { Lexer.Token }
%error { parseError }

%monad { Lexer.Alex  }
%lexer { Lexer.lexerCore } { Lexer.EOF }

%token
      fun    { Lexer.Fun _ }
      let    { Lexer.Let _ }
      letrec { Lexer.LetRec _ }
      case   { Lexer.Case _ }
      in     { Lexer.In _ }
      of     { Lexer.Of _ }
      Pack   { Lexer.Pack _ }
      num    { Lexer.Num _ $$ }
      var    { Lexer.Var _ $$ }
      altid  { Lexer.AltId _ $$ }
      '='    { Lexer.Sym _ "="  }
      ';'    { Lexer.Sym _ ";"  }
      '{'    { Lexer.Sym _ "{"  }
      '}'    { Lexer.Sym _ "{"  }
      '('    { Lexer.Sym _ "("  }
      ')'    { Lexer.Sym _ "}"  }
      ','    { Lexer.Sym _ ","  }
      '.'    { Lexer.Sym _ "."  }
      '\\'   { Lexer.Sym _ "\\" }
      '->'   { Lexer.Sym _ "->" }
      binop  { Lexer.Binop _ $$ }

%%

Prog : Prog ';' SC { $3 : $1  }
     | Prog ';'    { $1       }
     | SC          { [$1]     }
     | {- empty -} { []       }

vars : var         { [$1]  }
     | vars var    { $2:$1 }
     | {- empty -} { []    }

cvars : cvars ',' var { $3 : $1 }
      | var           { [$1]    }

SC : var vars '=' Expr { SC $1 $2 $4 }

def : var '=' Expr { Def $1 $3 }

defs : defs ';' def { $3 : $1 }
     | def          { [$1]    }

alt : altid vars  { Alt $1 $2  }

alts : alts ';' alt { $3 : $1 }
     | alt          { [$1]    }

Expr : Expr AExpr                  { App $1 $2      }
     | Expr binop Expr             { Binop $2 $1 $3 }
     | let defs in Expr            { Let $2 $4      }
     | letrec defs in Expr         { LetRec $2 $4   }
     | case Expr of alts           { Case $2 $4     }
     | fun '(' cvars ')' '->' Expr { Fun $3 $6      }
     | AExpr                       { Atomic $1      }

AExpr : var                      { Var $1     }
      | num                      { Num $1     }
      | Pack '{' num ',' num '}' { Pack $3 $5 }
      | '(' Expr ')'             { Paren $2   }

{
  
data Prog = Prog [SC]
data SC = SC String [String] Expr

data Expr
  = App Expr AExpr
  | Binop String Expr Expr
  | Let [Def] Expr
  | LetRec [Def] Expr
  | Case Expr [Alt]
  | Fun [String] Expr
  | Atomic AExpr

data AExpr
  = Var String
  | Num Int
  | Pack Int Int
  | Paren Expr

data Def = Def String Expr
-- The string here should be Int.
data Alt = Alt String [String]

parseError :: Lexer.Token -> Lexer.Alex a
parseError _ = error "Parse Error!"

}
