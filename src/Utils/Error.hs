{-# LANGUAGE FlexibleInstances #-}
-- | Error handling framework.
module Utils.Error where

-- | An exception can be either a vaild result or a failed state with
--   an error.
data Except error result 
  = Result result -- ^ Vaild result.
  | Failed error  -- ^ Failed state.
  deriving (Eq)

instance Show e => (Show (Except e String)) where
  show (Result result) = result
  show (Failed e) = show e

-- * The `Except` Monad

-- | Map over exceptions.
mapExcept :: (a -> b) -> (Except e a -> Except e b)
mapExcept f (Result r) = Result $ f r
mapExcept _ (Failed e) = Failed e

-- | Computations are vaild results.
returnExcept :: a -> Except e a
returnExcept r = Result r

-- | Bind over exceptions.  We can think of this as `c1 then (\r -> c2 r)`.
bindExcept :: Except e a -> (a -> Except e b) -> Except e b
bindExcept (Result r) f = f r
bindExcept (Failed e) _ = Failed e

-- | Function application for computations.
appExcept :: Except e (a -> b) -> Except e a -> Except e b
appExcept (Result f) (Result r) = Result $ f r
appExcept (Result f) (Failed e) = Failed e
appExcept (Failed e) _ = Failed e

instance (Functor (Except e)) where
  fmap = mapExcept

instance (Applicative (Except e)) where
  pure = returnExcept
  (<*>) = appExcept

instance (Monad (Except e)) where
  return = returnExcept
  (>>=) = bindExcept

instance MonadFail (Except String) where
  fail s = Failed s

