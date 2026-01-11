-- | 3-dimensional vectors.
-- |
-- | This module provides the `V3` type for 3D vector operations,
-- | including the cross product for computing perpendicular vectors.
module Linear.V3
  ( V3(..)
  -- * Cross product
  , cross
  , triple
  -- * Basis vectors
  , ex
  , ey
  , ez
  -- * Accessors
  , getX
  , getY
  , getZ
  ) where

import Prelude

import Data.Foldable (class Foldable)
import Data.Traversable (class Traversable)
import Linear.Epsilon (class Epsilon, nearZero)
import Linear.Metric (class Metric)
import Linear.Vector (class Additive)
import Data.Number (sqrt)

-- | A 3-dimensional vector.
-- |
-- | ```purescript
-- | origin = V3 0.0 0.0 0.0
-- | up = V3 0.0 1.0 0.0
-- | ```
data V3 a = V3 a a a

derive instance Eq a => Eq (V3 a)
derive instance Ord a => Ord (V3 a)

instance Show a => Show (V3 a) where
  show (V3 x y z) = "(V3 " <> show x <> " " <> show y <> " " <> show z <> ")"

instance Functor V3 where
  map f (V3 x y z) = V3 (f x) (f y) (f z)

instance Apply V3 where
  apply (V3 fx fy fz) (V3 x y z) = V3 (fx x) (fy y) (fz z)

instance Applicative V3 where
  pure a = V3 a a a

instance Bind V3 where
  bind (V3 x y z) f = V3 x' y' z'
    where
    V3 x' _ _ = f x
    V3 _ y' _ = f y
    V3 _ _ z' = f z

instance Monad V3

instance Foldable V3 where
  foldr f acc (V3 x y z) = f x (f y (f z acc))
  foldl f acc (V3 x y z) = f (f (f acc x) y) z
  foldMap f (V3 x y z) = f x <> f y <> f z

instance Traversable V3 where
  traverse f (V3 x y z) = V3 <$> f x <*> f y <*> f z
  sequence (V3 mx my mz) = V3 <$> mx <*> my <*> mz

instance Additive V3 where
  zero = V3 zero zero zero
  add (V3 x1 y1 z1) (V3 x2 y2 z2) = V3 (x1 + x2) (y1 + y2) (z1 + z2)
  sub (V3 x1 y1 z1) (V3 x2 y2 z2) = V3 (x1 - x2) (y1 - y2) (z1 - z2)
  lerp t (V3 x1 y1 z1) (V3 x2 y2 z2) =
    V3 (x1 + t * (x2 - x1)) (y1 + t * (y2 - y1)) (z1 + t * (z2 - z1))
  liftU2 f (V3 x1 y1 z1) (V3 x2 y2 z2) = V3 (f x1 x2) (f y1 y2) (f z1 z2)
  liftI2 f (V3 x1 y1 z1) (V3 x2 y2 z2) = V3 (f x1 x2) (f y1 y2) (f z1 z2)

instance Semiring a => Semigroup (V3 a) where
  append (V3 x1 y1 z1) (V3 x2 y2 z2) = V3 (x1 + x2) (y1 + y2) (z1 + z2)

instance Semiring a => Monoid (V3 a) where
  mempty = V3 zero zero zero

instance Epsilon a => Epsilon (V3 a) where
  nearZero (V3 x y z) = nearZero x && nearZero y && nearZero z

instance Metric V3 where
  dot (V3 x1 y1 z1) (V3 x2 y2 z2) = x1 * x2 + y1 * y2 + z1 * z2
  quadrance (V3 x y z) = x * x + y * y + z * z
  qd (V3 x1 y1 z1) (V3 x2 y2 z2) =
    let dx = x1 - x2
        dy = y1 - y2
        dz = z1 - z2
    in dx * dx + dy * dy + dz * dz
  distance (V3 x1 y1 z1) (V3 x2 y2 z2) =
    let dx = x1 - x2
        dy = y1 - y2
        dz = z1 - z2
    in sqrt (dx * dx + dy * dy + dz * dz)
  norm (V3 x y z) = sqrt (x * x + y * y + z * z)
  signorm v@(V3 x y z) =
    let n = sqrt (x * x + y * y + z * z)
    in if n == 0.0 then v else V3 (x / n) (y / n) (z / n)

-- | The cross product of two 3D vectors.
-- |
-- | The result is perpendicular to both input vectors, with magnitude
-- | equal to the area of the parallelogram they span.
-- |
-- | ```purescript
-- | cross (V3 1.0 0.0 0.0) (V3 0.0 1.0 0.0) = V3 0.0 0.0 1.0
-- | ```
cross :: forall a. Ring a => V3 a -> V3 a -> V3 a
cross (V3 x1 y1 z1) (V3 x2 y2 z2) = V3
  (y1 * z2 - z1 * y2)
  (z1 * x2 - x1 * z2)
  (x1 * y2 - y1 * x2)

-- | The scalar triple product of three vectors.
-- |
-- | This equals the signed volume of the parallelepiped formed by the vectors.
-- |
-- | ```purescript
-- | triple (V3 1.0 0.0 0.0) (V3 0.0 1.0 0.0) (V3 0.0 0.0 1.0) = 1.0
-- | ```
triple :: forall a. Ring a => V3 a -> V3 a -> V3 a -> a
triple u v w = dot3 u (cross v w)
  where
  dot3 (V3 x1 y1 z1) (V3 x2 y2 z2) = x1 * x2 + y1 * y2 + z1 * z2

-- | Unit vector along the x-axis.
ex :: forall a. Semiring a => V3 a
ex = V3 one zero zero

-- | Unit vector along the y-axis.
ey :: forall a. Semiring a => V3 a
ey = V3 zero one zero

-- | Unit vector along the z-axis.
ez :: forall a. Semiring a => V3 a
ez = V3 zero zero one

-- | Get the x component of a V3.
getX :: forall a. V3 a -> a
getX (V3 x _ _) = x

-- | Get the y component of a V3.
getY :: forall a. V3 a -> a
getY (V3 _ y _) = y

-- | Get the z component of a V3.
getZ :: forall a. V3 a -> a
getZ (V3 _ _ z) = z
