-- | Affine spaces and points.
-- |
-- | This module provides the `Affine` typeclass for affine spaces,
-- | where points and vectors are distinguished at the type level.
-- | Points can be subtracted to yield vectors, and vectors can be
-- | added to points, but points cannot be added to each other.
module Linear.Affine
  ( Point(..)
  , unP
  , class Affine
  , diff
  , moveBy
  , moveByNeg
  , (.-.)
  , (.+^)
  , (.-^)
  , origin
  , distanceA
  , qdA
  , lerpP
  ) where

import Prelude

import Data.Foldable (class Foldable, foldr, foldl, foldMap)
import Data.Newtype (class Newtype)
import Data.Traversable (class Traversable, traverse, sequence)
import Linear.Epsilon (class Epsilon, nearZero)
import Linear.Metric (class Metric, norm, quadrance)
import Linear.Vector (class Additive, add, sub, lerp)

-- | A point in an affine space.
-- |
-- | The `Point` newtype distinguishes points from vectors at the type level.
-- | This prevents nonsensical operations like adding two points together.
-- |
-- | ```purescript
-- | origin = P (V2 0.0 0.0)
-- | position = P (V2 3.0 4.0)
-- | ```
newtype Point f a = P (f a)

derive instance Newtype (Point f a) _

instance Eq (f a) => Eq (Point f a) where
  eq (P a) (P b) = a == b

instance Ord (f a) => Ord (Point f a) where
  compare (P a) (P b) = compare a b

instance Show (f a) => Show (Point f a) where
  show (P v) = "(P " <> show v <> ")"

instance Functor f => Functor (Point f) where
  map f (P v) = P (map f v)

instance Apply f => Apply (Point f) where
  apply (P f) (P v) = P (apply f v)

instance Applicative f => Applicative (Point f) where
  pure a = P (pure a)

instance Foldable f => Foldable (Point f) where
  foldr f z (P v) = foldr f z v
  foldl f z (P v) = foldl f z v
  foldMap f (P v) = foldMap f v

instance Traversable f => Traversable (Point f) where
  traverse f (P v) = P <$> traverse f v
  sequence (P v) = P <$> sequence v

instance (Additive f, Epsilon (f a)) => Epsilon (Point f a) where
  nearZero (P v) = nearZero v

-- | Unwrap a point to get the underlying vector.
unP :: forall f a. Point f a -> f a
unP (P v) = v

-- | An affine space is a set of points with associated difference vectors.
-- |
-- | Laws:
-- | - `p .+^ (q .-. p) = q`
-- | - `(p .+^ u) .+^ v = p .+^ (u ^+^ v)`
-- | - `p .-. p = zero`
class Additive d <= Affine p d | p -> d where
  -- | The vector from the first point to the second.
  diff :: forall a. Ring a => p a -> p a -> d a
  -- | Add a vector to a point.
  moveBy :: forall a. Semiring a => p a -> d a -> p a
  -- | Subtract a vector from a point.
  moveByNeg :: forall a. Ring a => p a -> d a -> p a

infixl 6 diff as .-.
infixl 6 moveBy as .+^
infixl 6 moveByNeg as .-^

-- | Points form an affine space over their underlying vectors.
instance Additive f => Affine (Point f) f where
  diff (P p1) (P p2) = sub p1 p2
  moveBy (P p) v = P (add p v)
  moveByNeg (P p) v = P (sub p v)

-- | The origin point (all coordinates zero).
-- |
-- | ```purescript
-- | origin :: Point V3 Number
-- | origin = P (V3 0.0 0.0 0.0)
-- | ```
origin :: forall f a. Applicative f => Semiring a => Point f a
origin = P (pure zero)

-- | The distance between two points.
-- |
-- | ```purescript
-- | distanceA (P (V2 0.0 0.0)) (P (V2 3.0 4.0)) = 5.0
-- | ```
distanceA :: forall p d. Affine p d => Metric d => p Number -> p Number -> Number
distanceA p1 p2 = norm (diff p1 p2)

-- | The squared distance between two points.
-- |
-- | More efficient than `distanceA` when you only need to compare distances.
-- |
-- | ```purescript
-- | qdA (P (V2 0.0 0.0)) (P (V2 3.0 4.0)) = 25.0
-- | ```
qdA :: forall p d. Affine p d => Metric d => p Number -> p Number -> Number
qdA p1 p2 = quadrance (diff p1 p2)

-- | Linear interpolation between two points.
-- |
-- | ```purescript
-- | lerpP 0.0 p1 p2 = p1
-- | lerpP 1.0 p1 p2 = p2
-- | lerpP 0.5 p1 p2 -- midpoint
-- | ```
lerpP :: forall p d. Affine p d => Functor d => Number -> p Number -> p Number -> p Number
lerpP t p1 p2 = p1 .+^ (map (_ * t) (diff p2 p1))
