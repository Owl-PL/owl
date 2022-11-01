{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GADTs #-}

import Data.Monoid

-- Nat : (|*| -> |*|) -> (|*| -> |*|) -> *
type Nat g h = forall x.forall a.g x a -> h x a

class HFunctor f where
    fhmap :: Nat g h -> forall x.Nat (f x g) (f x h)

