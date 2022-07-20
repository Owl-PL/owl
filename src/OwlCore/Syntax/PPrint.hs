module OwlCore.Syntax.PPrint where

import qualified OwlCore.Syntax.AST as AST

type IndentLevel = Int

data Markup a
  = Nil
  | Str a
  | Append (Markup a) (Markup a)
  | Newline IndentLevel

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

space :: IndentLevel -> String
space indent = take indent $ repeat ' '

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
  = "\n" ++ (space indent) ++ (flatten ms)

printMarkup :: Markup String -> String
printMarkup m = flatten [m]

(<++>) :: Markup a -> Markup a -> Markup a
m1 <++> m2 = Append m1 m2

(++>) :: a -> Markup a -> Markup a
s ++> m = (return s) <++> m

markup :: AST.Prog -> Markup String
markup = undefined

pprint :: AST.Prog -> String
pprint = printMarkup . markup
