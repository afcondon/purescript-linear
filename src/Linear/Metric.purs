-- | The Metric typeclass for inner products and norms.
-- |
-- | This module provides the `Metric` typeclass which extends `Additive`
-- | with operations for computing dot products, norms, and distances.
module Linear.Metric
  ( class Metric
  , dot
  , quadrance
  , qd
  , distance
  , norm
  , signorm
  , normalize
  , project
  ) where

import Prelude

import Linear.Epsilon (class Epsilon, nearZero)
import Linear.Vector (class Additive, sub, (^*), (^/))
import Data.Number (sqrt)

-- | A metric space with an inner product.
-- |
-- | Laws:
-- | - `dot v v >= 0` (positive semi-definite)
-- | - `dot v v = 0` implies `v = zero`
-- | - `dot u v = dot v u` (symmetry)
-- | - `dot (a *^ u) v = a * dot u v` (linearity)
class Additive f <= Metric f where
  -- | The inner (dot) product of two vectors.
  -- |
  -- | ```purescript
  -- | dot (V2 1.0 2.0) (V2 3.0 4.0) = 11.0  -- 1*3 + 2*4
  -- | ```
  dot :: forall a. Semiring a => f a -> f a -> a

  -- | The squared norm of a vector (quadrance from rational trigonometry).
  -- |
  -- | This is more efficient than `norm` when you only need to compare magnitudes.
  -- |
  -- | ```purescript
  -- | quadrance (V2 3.0 4.0) = 25.0  -- 3² + 4²
  -- | ```
  quadrance :: forall a. Semiring a => f a -> a

  -- | The squared distance between two vectors.
  -- |
  -- | ```purescript
  -- | qd (V2 0.0 0.0) (V2 3.0 4.0) = 25.0
  -- | ```
  qd :: forall a. Ring a => f a -> f a -> a

  -- | The Euclidean distance between two vectors.
  -- |
  -- | ```purescript
  -- | distance (V2 0.0 0.0) (V2 3.0 4.0) = 5.0
  -- | ```
  distance :: f Number -> f Number -> Number

  -- | The Euclidean norm (magnitude) of a vector.
  -- |
  -- | ```purescript
  -- | norm (V2 3.0 4.0) = 5.0
  -- | ```
  norm :: f Number -> Number

  -- | Convert a non-zero vector to a unit vector (signorm = "sign of norm").
  -- |
  -- | Returns the zero vector when given the zero vector.
  -- |
  -- | ```purescript
  -- | signorm (V2 3.0 4.0) = V2 0.6 0.8
  -- | signorm (V2 0.0 0.0) = V2 0.0 0.0
  -- | ```
  signorm :: f Number -> f Number

-- | Normalize a vector to unit length.
-- |
-- | Unlike `signorm`, this function handles the zero vector by returning
-- | zero rather than NaN.
-- |
-- | ```purescript
-- | normalize (V2 3.0 4.0) = V2 0.6 0.8
-- | normalize (V2 0.0 0.0) = V2 0.0 0.0
-- | ```
normalize :: forall f. Metric f => Epsilon Number => f Number -> f Number
normalize v
  | nearZero (quadrance v) = v
  | otherwise = signorm v

-- | Project the first vector onto the second.
-- |
-- | ```purescript
-- | project (V2 1.0 0.0) (V2 1.0 1.0) = V2 0.5 0.5
-- | ```
project :: forall f. Metric f => f Number -> f Number -> f Number
project u v = v ^* (dot u v / quadrance v)
