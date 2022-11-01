

record Monoid (A : Set) : Set where
  field
    mempty : A
    _<>_   : A → A → A

{-# NO_UNIVERSE_CHECK #-}
record Functor (F : Set → Set) : Set where
    field
      fmap : ∀ {A} {B} → (A → B) → F A → F B

{-# NO_UNIVERSE_CHECK #-}
record GrFunctor {E} (m : Monoid E) (F : E → Set → Set) : Set where
    field
      isFunctor : ∀ r → Functor (F r)

module GGADTS (E : Set)           (mon    : Monoid E) 
              (K : E → Set → Set) (grFunc : GrFunctor mon K) where

  I : E
  I = Monoid.mempty mon

  _⊗_ : E → E → E
  r1 ⊗ r2 = Monoid._<>_ mon r1 r2

  fmap : (r : E) → ∀{A}{B} → (A → B) → K r A → K r B
  fmap r = Functor.fmap (GrFunctor.isFunctor grFunc r)

  {-# NO_UNIVERSE_CHECK #-}
  record K-GradeAlg (X : E → Set → Set): Set where
    field
      structureMap : ∀ n m {A} → K m (X n A) → X (m ⊗ n) A
      
  

  