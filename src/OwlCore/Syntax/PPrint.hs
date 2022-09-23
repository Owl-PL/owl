-- | The pretty printer for OwlCore.
module OwlCore.Syntax.PPrint (pprint, pprintExpr) where

import qualified OwlCore.Syntax.AST as AST

import Control.Monad.State.Lazy

-- * A DSL for marking up data for efficent string conversion.
--   Recursivly converting data to a string usually requires a lot of
--   appending of strings.  This can lead to exponential blowup, and
--   thus, we use an intermediate representation called `Markup` that
--   sits between the data and its string representation.  We then
--   build a DSL for marking up data.
--
--   In order to control indention of data accross multiple lines we
--   will use state containing both a column and indention level.

-- | The type of columns.
type Column = Int
-- | The type of the indention level.
type IndentLevel = Int

-- | The `Markup` data type represents a list of strings using
--   explicit append and newlines
data Markup a
  = Nil                           -- ^ The empty list.
  | Str a                         -- ^ A string.
  | Append (Markup a) (Markup a)  -- ^ Appending two lists.
  | Newline IndentLevel           -- ^ A newline between lists.
  deriving Show

-- ** `Markup` is a monad.

-- | The return of a monad.
ret :: a -> Markup a
ret st = Str st

-- | The bind of a monad.
bind :: Markup a -> (a -> Markup b) -> Markup b
bind Nil _ = Nil
bind (Newline indent) _ = Newline indent
bind (Str st) f = f st
bind (Append m1 m2) f = Append (bind m1 f) (bind m2 f)

-- | `Markup` perseves functions.
seqM :: Markup (a -> b) -> Markup a -> Markup b
seqM Nil              _                = Nil
seqM (Newline indent) _                = Newline indent
seqM (Str f)          Nil              = Nil
seqM (Str f)          (Newline indent) = Newline indent
seqM (Str f)          (Str s)          = Str (f s)
seqM f@(Str _)        (Append m1 m2)   = Append (seqM f m1) (seqM f m2)
seqM (Append m1 m2)   m                = Append (seqM m1 m) (seqM m2 m)

-- | `Markup` is a functor.
fmapM :: (a -> b) -> (Markup a) -> (Markup b)
fmapM f Nil = Nil
fmapM f (Newline i) = Newline i
fmapM f (Str s) = Str (f s)
fmapM f (Append m1 m2) = Append (fmapM f m1) (fmapM f m2)

instance (Functor Markup) where
  fmap = fmapM

instance (Applicative Markup) where
  pure = ret
  (<*>) = seqM

instance (Monad Markup) where
  return = ret
  (>>=) = bind

-- ** The DSL.
--
-- Making it easier to makeup data can be done through a DSL for
-- `Markup`.  This DSL will require keeping track of the current
-- column we are at during the markup process, the indention level,
-- and a result accumulator.
--
-- The DSL also keeps appending markup using `Append` implicit.

-- | The state of the markup process.
type MarkupState a r = State
                       (Column,      -- ^ The current column.
                        IndentLevel, -- ^ The current indention level.
                        Markup a) r  -- ^ The accumulator.

-- | Increases the current indention level by `i` starting at the
--   current column.
indentMany :: IndentLevel -> MarkupState a ()
indentMany i = do
  (col, indent, m) <- get
  put (col, col + i, m)

-- | Sets the indention level to the current column.
indent :: MarkupState a ()
indent = indentMany 0

-- | Decreases the current indention level and current column by `i`.
unindentMany :: Int -> MarkupState a ()
unindentMany i = do
  (col, indent, m) <- get
  let newIndent = indent - i
  let newCol = col - i
  
  if newIndent >= 0
  then put (newCol,newIndent, m)
  else return ()

-- | Decreases the current indention level and current column by 1.
unindent :: MarkupState a ()
unindent = unindentMany 1

-- | Appends the string `s` to the current markup, and increaes the
--   current column by the length of the string.
string :: String -> MarkupState String ()
string s = do
  (col, indent, m) <- get
  put (col + (length s), indent, Append m (Str s))

-- | Appends a newline to the current markup, but does not modify the
--   indention level or the current column.
newline :: MarkupState a ()
newline = do
  (col, indent, m) <- get
  put $ (indent, indent, Append m (Newline indent))

-- | Appends a space to the current markup.  This is an alias to
-- `string " "`.
space :: MarkupState String ()
space = string " "

-- | Returns the current column.
getColumn :: MarkupState String Column
getColumn = do (c, _, _) <- get
               return c
               
-- | Sets the current column to `c`, but doesn't modify the indention
--   level or the accumulator.
setColumn :: Column -> MarkupState String ()
setColumn c = do (_, indent, m) <- get
                 put (c, indent, m)

-- | Returns the current indention level.
getIndent :: MarkupState String Column
getIndent = do (_, i, _) <- get
               return i

-- | Sets the current indention level to `i`, but doesn't modify the
-- current column or the accumulator.
setIndent :: Column -> MarkupState String ()
setIndent i = do (c, _, m) <- get
                 put (c, i, m)

-- * AST Markup and Pretty Printing

-- | Adds parens around the markup.
markupParenExpr :: AST.Expr -> MarkupState String ()
markupParenExpr (AST.Atomic ae) = do
  markupAExpr ae
markupParenExpr e = do
  string "("
  markupExpr e
  string ")"

-- | Marks up an atomic expression.
markupAExpr :: AST.AExpr -> MarkupState String ()
markupAExpr (AST.Var x) = string x
markupAExpr (AST.Num i) = string . show $ i
markupAExpr (AST.Pack i1 i2) =
  do string "Pack{"
     (string . show $ i1)
     string ", "
     (string . show $ i2)
     string "}"

markupAExpr (AST.Paren (AST.Atomic e)) = markupAExpr e
markupAExpr (AST.Paren e) = markupParenExpr e

-- | Marks up a definition.
markupDef :: AST.Def -> MarkupState String ()
markupDef (AST.Def name body@(AST.Let _ _)) =
  do string name
     string " = "     
     markupExpr . AST.parenExpr $ body     

markupDef (AST.Def name body@(AST.LetRec _ _)) =
  do string name
     string " = "     
     markupExpr . AST.parenExpr $ body     

markupDef (AST.Def name body) =
  do string name
     string " = "     
     markupExpr body

-- | Marks up a list of definitions.
markupDefs :: [AST.Def] -> MarkupState String ()
markupDefs [] = return ()
markupDefs [def] = markupDef def
markupDefs (def : defs) =
  do markupDefs defs     
     string ";"
     newline
     markupDef def
     
-- | Marks up an alternative.
markupAlt :: AST.Alt -> MarkupState String ()
markupAlt (AST.Alt altid vars body@(AST.Case _ _))  =
  do string "<"
     string . show $ altid
     string "> "
     markupPVar vars
     string " -> "
     markupExpr . AST.parenExpr $ body

markupAlt (AST.Alt altid vars body)  =
  do string "<"
     string . show $ altid
     string "> "
     markupPVar vars
     string " -> "
     markupExpr body     

-- | Marks up a list of alternatives.
markupAlts :: [AST.Alt] -> MarkupState String ()
markupAlts []    = return ()
markupAlts [alt] = markupAlt alt
markupAlts (alt : alts) =
  do markupAlts alts
     string ";"
     newline     
     markupAlt alt

-- | Marks up an expression.
markupExpr :: AST.Expr -> MarkupState String ()

markupExpr (AST.App e ae) =
  do markupParenExpr e
     space
     markupAExpr ae

markupExpr (AST.Binop op e1 e2) =
  do markupParenExpr e1
     space
     string (AST.opToStr op)
     space
     markupParenExpr e2

-- Should never be hit if given a well-formed program.
markupExpr (AST.Let [] e) = return ()

markupExpr (AST.Let [def] e) =
  do string "let "
     markupDef def
     string " in "
     markupExpr e

markupExpr (AST.Let defs e) =
  do string "let "
     col <- getColumn
     indent
     markupDefs defs
     newline     
     string "in "
     markupExpr e
     setColumn col

-- Should never be hit if given a well-formed program.
markupExpr (AST.LetRec [] e) = return ()

markupExpr (AST.LetRec [def] e) =
  do string "letrec "
     markupDef def
     string " in "
     markupExpr e

markupExpr (AST.LetRec defs e) =
  do string "letrec "
     col <- getColumn
     indent
     markupDefs defs
     newline
     string "in "
     markupExpr e
     setColumn col

markupExpr (AST.Case e@(AST.Atomic _) [alt]) =
  do string "case "
     markupExpr e
     string " of "  
     markupAlt alt

markupExpr (AST.Case e [alt]) =
  do string "case "
     markupParenExpr e
     string " of "  
     markupAlt alt
     
markupExpr (AST.Case e alts) =
  do string "case "
     indent
     i <- getIndent
     markupExpr $ AST.parenExpr e
     setIndent i
     string " of "     
     newline     
     markupAlts alts     

markupExpr (AST.Fun binders e@(AST.Fun _ _)) =
  do string "fun "
     markupBinders binders
     string " -> "
     markupExpr e

markupExpr (AST.Fun binders e) =
  do string "fun "
     markupBinders binders
     string " -> "
     markupParenExpr e     

markupExpr (AST.Atomic ae) = markupAExpr ae

-- | Marks up a list of binder names.
markupBinders :: [AST.Name] -> MarkupState String ()
markupBinders [b] = string b
markupBinders binders = do
  string "("
  markupBinders' binders
  string ")"
  where
    markupBinders' [] = string ""
    markupBinders' [b] = string b
    markupBinders' (b : bs) = do
      markupBinders' bs  
      string ", "
      string b

-- | Marks up a list of pattern variables.  
markupPVar :: [AST.Name] -> MarkupState String ()
markupPVar [] = string ""
markupPVar [v] = string v
markupPVar (v : vs) = do
  markupPVar vs  
  string " "
  string v

-- | Marks up a super combinator.  
markupSC :: AST.SC -> MarkupState String ()
markupSC (AST.SC name args body) =
  do string name
     space
     (string.unwords $ args)
     string " = "
     markupExpr body

-- | Converts a whole program into the intermediate representation
--   `Markup` that makes converting into a string more efficent.
markup :: AST.Prog -> MarkupState String ()
markup [] = return ()
markup [sc] = markupSC sc
markup (sc : prog) =
  do markup prog
     string ";"
     newline
     markupSC sc          

-- | Runs the markup on expressions.
runMarkupExpr :: AST.Expr -> Markup String
runMarkupExpr expr =
  case execState (markupExpr expr) (0, 0, Nil) of
    (_,_,m) -> m

-- | Converts the markup to a string.
printMarkup :: Markup String -> String
printMarkup m = flatten [m]
  where
    indentLevel :: IndentLevel -> String
    indentLevel indent = take indent $ repeat ' '

    flatten :: [Markup String] -> String
    flatten [] = ""
    flatten (Nil : ms) = flatten ms
    flatten (Str st : ms) = st ++ (flatten ms)
    flatten (Append m1 m2 : ms) = flatten (m1 : m2 : ms)
    flatten ((Newline indent) : ms)
      = "\n" ++ (indentLevel indent) ++ (flatten ms)

-- | The pretty printer for expressions.
pprintExpr :: AST.Expr -> String
pprintExpr = printMarkup . runMarkupExpr

-- | Runs the markup on whole programs.
runMarkup :: AST.Prog -> Markup String
runMarkup prog =
  case execState (markup prog) (0, 0, Nil) of
    (_,_,m) -> m

-- | The pretty printer for whole programs.
pprint :: AST.Prog -> String
pprint = printMarkup . runMarkup  

