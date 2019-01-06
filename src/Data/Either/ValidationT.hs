-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Either.ValidationT
-- Copyright   :  (c) 2019 Chris Allen, Edward Kmett, Kostiantyn Rybnikov
-- License     :  BSD-style
--
-- Maintainer  :  ekmett@gmail.com
-- Stability   :  provisional
-- Portability :  portable
--
-- Transformer version of 'Validation'. Similar to ExceptT, but
-- accumulates errors instead of exiting early with the first one.
--
-----------------------------------------------------------------------------
module Data.Either.ValidationT
  ( ValidationT(..)
  , runValidationT
  ) where

import Data.Either.Validation
import Control.Applicative (liftA2)

-- | 'ValidationT' is 'Either' with a Left that is a 'Monoid'
newtype ValidationT e m a =
  ValidationT (m (Validation e a))

-- | The inverse of 'ValidationT'.
runValidationT :: ValidationT e m a -> m (Validation e a)
runValidationT (ValidationT m) = m

{-# INLINE runValidationT #-}
instance (Functor m) => Functor (ValidationT e m) where
  fmap f = ValidationT . fmap (fmap f) . runValidationT
  {-# INLINE fmap #-}

instance (Semigroup e, Functor m, Applicative m) =>
         Applicative (ValidationT e m) where
  pure a = ValidationT $ pure (Success a)
  {-# INLINE pure #-}
  ValidationT f <*> ValidationT v = ValidationT $ liftA2 (<*>) f v
  {-# INLINEABLE (<*>) #-}
