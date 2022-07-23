module OwlCore.Syntax.PPrint where

import qualified OwlCore.Syntax.AST as AST

import Control.Monad.State.Lazy

type Column = Int
type IndentLevel = Int

data Markup a
  = Nil
  | Str a
  | Append (Markup a) (Markup a)
  | Newline IndentLevel
  deriving Show

ret :: a -> Markup a
ret st = Str st

bind :: Markup a -> (a -> Markup b) -> Markup b
bind Nil _ = Nil
bind (Newline indent) _ = Newline indent
bind (Str st) f = f st
bind (Append m1 m2) f = Append (bind m1 f) (bind m2 f)

seqM :: Markup (a -> b) -> Markup a -> Markup b
seqM Nil              _                = Nil
seqM (Newline indent) _                = Newline indent
seqM (Str f)          Nil              = Nil
seqM (Str f)          (Newline indent) = Newline indent
seqM (Str f)          (Str s)          = Str (f s)
seqM f@(Str _)        (Append m1 m2)   = Append (seqM f m1) (seqM f m2)
seqM (Append m1 m2)   m                = Append (seqM m1 m) (seqM m2 m)

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

indentLevel :: IndentLevel -> String
indentLevel indent = take indent $ repeat ' '

printMarkup :: Markup String -> String
printMarkup m = flatten [m]
  where
    flatten :: [Markup String] -> String
    flatten [] = ""
    flatten (Nil : ms) = flatten ms
    flatten (Str st : ms) = st ++ (flatten ms)
    flatten (Append m1 m2 : ms) = flatten (m1 : m2 : ms)
    flatten ((Newline indent) : ms)
      = "\n" ++ (indentLevel indent) ++ (flatten ms)

-- A state monad that keeps track of the current indention level
-- during markup.
type MarkupState a r = State (Column, IndentLevel, Markup a) r

indentMany :: IndentLevel -> MarkupState a ()
indentMany i = do
  (col, indent, m) <- get
  put (col, col + i, m)

indent :: MarkupState a ()
indent = indentMany 0

unindentMany :: Int -> MarkupState a ()
unindentMany i = do
  (col, indent, m) <- get
  let newIndent = indent - i
  let newCol = col - i
  
  if newIndent >= 0
  then put (newCol,newIndent, m)
  else return ()

unindent :: MarkupState a ()
unindent = unindentMany 1

string :: String -> MarkupState String ()
string s = do
  (col, indent, m) <- get
  put (col + (length s), indent, Append m (Str s))

newline :: MarkupState a ()
newline = do
  (col, indent, m) <- get
  put $ (indent, indent, Append m (Newline indent))

space :: MarkupState String ()
space = string " "

markupAExpr :: AST.AExpr -> MarkupState String ()
markupAExpr (AST.Var x) = string x
markupAExpr (AST.Num i) = string . show $ i
markupAExpr (AST.Pack i1 i2) =
  do string "Pack {"
     (string . show $ i1)
     string ", "
     (string . show $ i2)
     string "}"
  
markupAExpr (AST.Paren e) =
  do string "("
     markupExpr e
     string ")"

markupDef :: AST.Def -> MarkupState String ()
markupDef (AST.Def name body) =
  do string name
     string " = "     
     markupExpr $ AST.parenExpr body

markupDefs :: [AST.Def] -> MarkupState String ()
markupDefs [] = return ()
markupDefs [def] = markupDef def
markupDefs (def : defs) =
  do markupDefs defs     
     string ";"
     newline
     markupDef def
     

markupAlt :: AST.Alt -> MarkupState String ()
markupAlt (AST.Alt altid vars body)  =
  do string "<"
     string . show $ altid
     string "> "
     markupPVar vars
     string " -> "
     markupExpr $ AST.parenExpr body

markupAlts :: [AST.Alt] -> MarkupState String ()
markupAlts []    = return ()
markupAlts [alt] = markupAlt alt
markupAlts (alt : alts) =
  do markupAlts alts
     string ";"
     newline     
     markupAlt alt

getColumn :: MarkupState String Column
getColumn = do (c, _, _) <- get
               return c

setColumn :: Column -> MarkupState String ()
setColumn c = do (_, indent, m) <- get
                 put (c, indent, m)

markupExpr :: AST.Expr -> MarkupState String ()

markupExpr (AST.App e ae) =
  do markupExpr $ AST.parenExpr e
     space
     markupAExpr ae
  
markupExpr (AST.Binop op e1 e2) =
  do markupExpr $ AST.parenExpr e1
     space
     string op
     space
     markupExpr $ AST.parenExpr e2

-- Should never be hit if given a well-formed program.
markupExpr (AST.Let [] e) = return ()

markupExpr (AST.Let [def] e) =
  do string "let "
     markupDef def
     string " in "
     markupExpr e

markupExpr (AST.Let defs e) =
  do string "let "
     indent
     markupDefs defs
     newline     
     string "in "
     markupExpr e

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
  
markupExpr (AST.Case e alts) =
  do string "case "
     col <- getColumn
     indent
     markupExpr $ AST.parenExpr e
     string " of "
     newline
     markupAlts alts
     setColumn col    

markupExpr (AST.Fun binders e@(AST.Fun _ _)) =
  do string "fun "
     string "("
     markupBinders binders
     string ")"
     string " -> "
     markupExpr e

markupExpr (AST.Fun binders e) =
  do string "fun "
     string "("
     markupBinders binders
     string ")"
     string " -> "
     markupExpr $ AST.parenExpr e     

markupExpr (AST.Atomic ae) = markupAExpr ae

markupBinders :: [String] -> MarkupState String ()
markupBinders [] = string ""
markupBinders [b] = string b
markupBinders (b : bs) = do
  markupBinders bs  
  string ", "
  string b
  
markupPVar :: [String] -> MarkupState String ()
markupPVar [] = string ""
markupPVar [v] = string v
markupPVar (v : vs) = do
  markupPVar vs  
  string " "
  string v
  
markupSC :: AST.SC -> MarkupState String ()
markupSC (AST.SC name args body) =
  do string name
     space
     (string.unwords $ args)
     string " = "
     markupExpr body
  
markup :: AST.Prog -> MarkupState String ()
markup [] = return ()
markup [sc] = markupSC sc
markup (sc : prog) =
  do markup prog
     string ";"
     newline
     markupSC sc          

runMarkupExpr :: AST.Expr -> Markup String
runMarkupExpr expr =
  case execState (markupExpr expr) (0, 0, Nil) of
    (_,_,m) -> m

pprintExpr :: AST.Expr -> String
pprintExpr = printMarkup . runMarkupExpr
     
runMarkup :: AST.Prog -> Markup String
runMarkup prog =
  case execState (markup prog) (0, 0, Nil) of
    (_,_,m) -> m

pprint :: AST.Prog -> String
pprint = printMarkup . runMarkup  

