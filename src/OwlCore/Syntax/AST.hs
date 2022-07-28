-- | OwlCore's abstract syntax.
module OwlCore.Syntax.AST where

-- | The type of variable names.
type Name = String

-- | The type of whole programs: a list of super combinators.
type Prog = [SC]

-- | Super combinators.
data SC = SC
  Name   -- ^ The name of the combinator.
  [Name] -- ^ The list of variables.
  Expr   -- ^ The body of the combinator.
  deriving (Show,Eq)

-- | Expressions.
data Expr
  = App Expr AExpr          -- ^ Function application.
  | Binop String Expr Expr  -- ^ Binary operations.
  | Let [Def] Expr          -- ^ Let expressions.
  | LetRec [Def] Expr       -- ^ Recursive let expressions.
  | Case Expr [Alt]         -- ^ Case expressions.
  | Fun [String] Expr       -- ^ Functions.
  | Atomic AExpr            -- ^ Atomic expressions.
  deriving (Show,Eq)  

-- | The unique identifier of constructors.
type Tag = Int
-- | The airty of constructors.
type Arity = Int

-- | Atomic expressions.
data AExpr
  = Var String   -- ^ Variables.
  | Num Int      -- ^ Numbers.

  -- | Datatype constructors.
  | Pack
    Tag   -- ^ The tag that uniquely identifies the constructor.
    Arity -- ^ The arity of the constructor.
  
  | Paren Expr   -- ^ Parenthesized expressions.
  deriving (Show,Eq)  

-- | A definition introduced by either a `Expr.Let` or `Expr.LetRec`.
data Def
  = Def String -- ^ The variable name of the definition.
        Expr   -- ^ The body of the definition.
  deriving (Show,Eq)

-- | An alternative in a case expression.
data Alt = Alt
           Tag      -- The constructor we are matching on.
           [Name]   -- The parameters of the constructor.
           Expr     -- The body of the alternative.
  deriving (Show,Eq)

-- | Decides when to add parens around expressions when pretty printing.
parenExpr :: Expr -> Expr
parenExpr e@(App _ _)     = Atomic (Paren e)
parenExpr e@(Binop _ _ _) = Atomic (Paren e)
parenExpr e@(Let _ _)     = Atomic (Paren e)
parenExpr e@(LetRec _ _)  = Atomic (Paren e)
parenExpr e@(Case _ _)    = Atomic (Paren e)
parenExpr e@(Fun _ _)     = Atomic (Paren e)
parenExpr e               = e
