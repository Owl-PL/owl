module OwlCore.Syntax.AST where

type Prog = [SC]

data SC = SC String [String] Expr
  deriving (Show,Eq)

data Expr
  = App Expr AExpr
  | Binop String Expr Expr
  | Let [Def] Expr
  | LetRec [Def] Expr
  | Case Expr [Alt]
  | Fun [String] Expr
  | Atomic AExpr
  deriving (Show,Eq)  

data AExpr
  = Var String
  | Num Int
  | Pack Int Int
  | Paren Expr
  deriving (Show,Eq)  

data Def = Def String Expr
  deriving (Show,Eq)

data Alt = Alt Int [String] Expr
  deriving (Show,Eq)


