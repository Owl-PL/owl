module OwlCore.Syntax.PPrint where

import qualified OwlCore.Syntax.AST as AST

import Control.Monad.State.Lazy

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

flatten :: [Markup String] -> String

flatten []
  = ""
  
flatten (Nil : ms)
  = flatten ms

flatten (Str st : ms)
  = st ++ (flatten ms)

flatten (Append m1 m2 : ms)
  = flatten (m1 : m2 : ms)

flatten ((Newline indent) : ms)
  = "\n" ++ (indentLevel indent) ++ (flatten ms)

printMarkup :: Markup String -> String
printMarkup m = flatten [m]

-- A state monad that keeps track of the current indention level
-- during markup.
type MarkupState a r = State (IndentLevel, Markup a) r

indentMany :: IndentLevel -> MarkupState a ()
indentMany i = do
  (indent, m) <- get
  put ((indent + i), m)

indent :: MarkupState a ()
indent = indentMany 1

string :: a -> MarkupState a ()
string s = do
  (indent, m) <- get
  put (indent, Append m (Str s))

newline :: MarkupState a ()
newline = do
  (indent, m) <- get
  put $ (indent, Append m (Newline indent))

semicolon :: MarkupState String ()
semicolon = string ";"

equal :: MarkupState String ()
equal = string "="

space :: MarkupState String ()
space = string " "

markupAExpr :: AST.AExpr -> MarkupState String ()
markupAExpr (AST.Var x) = string x
markupAExpr (AST.Num i) = string . show $ i
markupAExpr (AST.Pack i1 i2) =
  do string "Pack"
     space
     string "{"
     (string . show $ i1)
     string ","
     space
     (string . show $ i2)
     string "}"
  
markupAExpr (AST.Paren e) =
  do string "("
     markupExpr e
     string ")"

markupDef :: AST.Def -> MarkupState String ()
markupDef (AST.Def name body) =
  do string name
     space
     equal
     space
     markupExpr body

markupDefs :: [AST.Def] -> MarkupState String ()
markupDefs [] = return ()
markupDefs [def] = markupDef def
markupDefs (def : defs) =
  do markupDef def
     semicolon
     indentMany 4
     newline
     markupDefs defs

markupAlt :: AST.Alt -> MarkupState String ()
markupAlt (AST.Alt altid vars body)  =
  do string "<"
     string altid
     string ">"
     space
     (string . unwords $ vars)
     space
     string "->"
     space
     markupExpr body

markupAlts :: [AST.Alt] -> MarkupState String ()
markupAlts []    = return ()
markupAlts [alt] = markupAlt alt
markupAlts (alt : alts) =
  do markupAlt alt
     semicolon
     newline
     markupAlts alts

markupExpr :: AST.Expr -> MarkupState String ()

markupExpr (AST.App e ae) =
  do markupExpr e
     space
     markupAExpr ae
  
markupExpr (AST.Binop op e1 e2) =
  do markupExpr e1
     space
     string op
     space
     markupExpr e2

markupExpr (AST.Let [def] e) =
  do string "let"
     space
     markupDef def
     space
     string "in"
     space
     markupExpr e

markupExpr (AST.LetRec defs e) =
  do string "letrec"
     space
     markupDefs defs
     space
     string "in"
     space
     markupExpr e
  
markupExpr (AST.Case e alts) =
  do string "case"
     space
     markupExpr e
     space
     string "of"
     space
     markupAlts alts

markupExpr (AST.Fun binders e) =
  do string "fun"
     space
     string "("
     (string . unwords $ binders)
     string ")"
     space
     string "->"
     space
     markupExpr e

markupExpr (AST.Atomic ae) = markupAExpr ae

markupSC :: AST.SC -> MarkupState String ()
markupSC (AST.SC name args body) =
  do string name
     space
     (string.unwords $ args)
     space
     equal
     space
     markupExpr body
  
markup :: AST.Prog -> MarkupState String ()
markup [] = return ()
markup [sc] = markupSC sc
markup (sc : prog) =
  do markup prog
     semicolon
     newline
     markupSC sc          
     

runMarkup :: AST.Prog -> Markup String
runMarkup prog = snd $ execState (markup prog) (0, Nil)

pprint :: AST.Prog -> String
pprint = printMarkup . runMarkup  

-- example = show $ evalState (
--   string "1"
--     <++> newline
--     <++> indent
--     <++> string "2") 1
