-- | Approximate equality testing for floating-point values.
-- |
-- | This module provides the `Epsilon` typeclass for testing whether
-- | values are "near zero", useful for numerical algorithms that need
-- | to handle floating-point imprecision.
module Linear.Epsilon
  ( class Epsilon
  , nearZero
  ) where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Ord (abs)

-- | A typeclass for types that support approximate zero testing.
-- |
-- | The `nearZero` function provides a "fairly subjective test to see
-- | if a quantity is near zero" (as Ed Kmett puts it).
class Epsilon a where
  nearZero :: a -> Boolean

instance Epsilon Number where
  nearZero a = abs a <= 1.0e-12

instance Epsilon Int where
  nearZero a = a == 0

-- | For Maybe, Nothing is considered near zero, and Just delegates.
instance Epsilon a => Epsilon (Maybe a) where
  nearZero Nothing = true
  nearZero (Just a) = nearZero a
