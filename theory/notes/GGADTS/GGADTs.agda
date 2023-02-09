

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

  open import Data.Product

  K-GradeAlg : Set₁
  K-GradeAlg = Σ[ X ∈ (E → Set → Set) ](∀ n m {A} → K m (X n A) → X (m ⊗ n) A)

K-GradeAlgHom : K-GradeAlg → K-GradeAlg → Set₁
K-GradeAlgHom (X , _) (Y , _) = Σ[ f ∈ (∀ n {A} → X n A → Y n A) ]({!   !})