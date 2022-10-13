{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GADTs #-}

-- Nat : (|*| -> |*|) -> (|*| -> |*|) -> *
type Nat g h = forall a.g a -> h a

class HFunctor f where
    fhmap :: Nat g h -> Nat (f g) (f h)

-- G :  ((|*| -> *) -> |*| -> *)
--   -> (|*| -> |*|)
--   -> |*|
--   -> *
--
-- f : (|*| -> *) -> |*| -> *
-- h : |*| -> |*|
data G f h a where
    GCon :: f (G f h) a -> G f h (h a)

-- Consider G f h as a functor G_{f,h} : |*| -> * where
-- f is a functor and h is a functor.
--
-- Then GCon is a natural transformation between 
-- (f G_{f,h}) -> (G_{f,h} h)

-- Lan_h (f G_{f,h}) is the 
-- left Kan extension of the functor (f G_{f,h}) a long h
--
-- Hom(f G_{f, h},h'(-))) = Hom(Lan_h (f G_{f,h}), -)
--
-- where h'(g) = h ; g
--
-- To apply the isomorphism above to GCon we set g = G_{f,h}. Then we obtain a 
-- functor 
-- 
-- Lan_h (f G_{f,h}) -> G_{f, h}
--
-- This fits the form of a usual algebraic data type, and it's iso. to GCon.

data Eql a b where 
    Refl :: Eql a a

data Lan h g c = forall b.Lan (Eql (h b) c, g b)

-- Given the above definition the iso. type of GCon:
--
-- LGCon : Lan_h (f G_{f,h}) -> G_{f, h}
--
-- is unfolded as 
--
-- forall b.Lan (Eql (h b) c, (f G_{f, h}) b) -> G_{f, h}
--
-- But this is equivalent to the type:
--
-- exist b.(((h b) = c, ((f G_{f, h}) b) -> G_{f, h})
--
-- I can write this type down in Owl directly.

newtype Mu f a = In (f (Mu f) a)

-- This tells us that f is the functor whose f-algebras will interpret G_{f,h}
-- as the carrier of the initial algebra in the category of f-algebras. That is,
-- (G_{f,h},LGCon) is the initial algebra.

