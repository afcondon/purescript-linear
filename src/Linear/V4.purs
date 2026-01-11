-- | 4-dimensional vectors.
-- |
-- | This module provides the `V4` type for 4D vector operations,
-- | commonly used for homogeneous coordinates in 3D graphics.
module Linear.V4
  ( V4(..)
  -- * Homogeneous coordinates
  , vector
  , point
  , normalizePoint
  -- * Basis vectors
  , ex
  , ey
  , ez
  , ew
  -- * Accessors
  , getX
  , getY
  , getZ
  , getW
  ) where

import Prelude

import Data.Foldable (class Foldable)
import Data.Traversable (class Traversable)
import Linear.Epsilon (class Epsilon, nearZero)
import Linear.Metric (class Metric)
import Linear.V3 (V3(..))
import Linear.Vector (class Additive)
import Data.Number (sqrt)

-- | A 4-dimensional vector.
-- |
-- | Often used for homogeneous coordinates where (x, y, z, w) represents
-- | the 3D point (x/w, y/w, z/w).
-- |
-- | ```purescript
-- | origin = V4 0.0 0.0 0.0 1.0
-- | direction = V4 1.0 0.0 0.0 0.0
-- | ```
data V4 a = V4 a a a a

derive instance Eq a => Eq (V4 a)
derive instance Ord a => Ord (V4 a)

instance Show a => Show (V4 a) where
  show (V4 x y z w) = "(V4 " <> show x <> " " <> show y <> " " <> show z <> " " <> show w <> ")"

instance Functor V4 where
  map f (V4 x y z w) = V4 (f x) (f y) (f z) (f w)

instance Apply V4 where
  apply (V4 fx fy fz fw) (V4 x y z w) = V4 (fx x) (fy y) (fz z) (fw w)

instance Applicative V4 where
  pure a = V4 a a a a

instance Bind V4 where
  bind (V4 x y z w) f = V4 x' y' z' w'
    where
    V4 x' _ _ _ = f x
    V4 _ y' _ _ = f y
    V4 _ _ z' _ = f z
    V4 _ _ _ w' = f w

instance Monad V4

instance Foldable V4 where
  foldr f acc (V4 x y z w) = f x (f y (f z (f w acc)))
  foldl f acc (V4 x y z w) = f (f (f (f acc x) y) z) w
  foldMap f (V4 x y z w) = f x <> f y <> f z <> f w

instance Traversable V4 where
  traverse f (V4 x y z w) = V4 <$> f x <*> f y <*> f z <*> f w
  sequence (V4 mx my mz mw) = V4 <$> mx <*> my <*> mz <*> mw

instance Additive V4 where
  zero = V4 zero zero zero zero
  add (V4 x1 y1 z1 w1) (V4 x2 y2 z2 w2) = V4 (x1 + x2) (y1 + y2) (z1 + z2) (w1 + w2)
  sub (V4 x1 y1 z1 w1) (V4 x2 y2 z2 w2) = V4 (x1 - x2) (y1 - y2) (z1 - z2) (w1 - w2)
  lerp t (V4 x1 y1 z1 w1) (V4 x2 y2 z2 w2) =
    V4 (x1 + t * (x2 - x1)) (y1 + t * (y2 - y1)) (z1 + t * (z2 - z1)) (w1 + t * (w2 - w1))
  liftU2 f (V4 x1 y1 z1 w1) (V4 x2 y2 z2 w2) = V4 (f x1 x2) (f y1 y2) (f z1 z2) (f w1 w2)
  liftI2 f (V4 x1 y1 z1 w1) (V4 x2 y2 z2 w2) = V4 (f x1 x2) (f y1 y2) (f z1 z2) (f w1 w2)

instance Semiring a => Semigroup (V4 a) where
  append (V4 x1 y1 z1 w1) (V4 x2 y2 z2 w2) = V4 (x1 + x2) (y1 + y2) (z1 + z2) (w1 + w2)

instance Semiring a => Monoid (V4 a) where
  mempty = V4 zero zero zero zero

instance Epsilon a => Epsilon (V4 a) where
  nearZero (V4 x y z w) = nearZero x && nearZero y && nearZero z && nearZero w

instance Metric V4 where
  dot (V4 x1 y1 z1 w1) (V4 x2 y2 z2 w2) = x1 * x2 + y1 * y2 + z1 * z2 + w1 * w2
  quadrance (V4 x y z w) = x * x + y * y + z * z + w * w
  qd (V4 x1 y1 z1 w1) (V4 x2 y2 z2 w2) =
    let dx = x1 - x2
        dy = y1 - y2
        dz = z1 - z2
        dw = w1 - w2
    in dx * dx + dy * dy + dz * dz + dw * dw
  distance (V4 x1 y1 z1 w1) (V4 x2 y2 z2 w2) =
    let dx = x1 - x2
        dy = y1 - y2
        dz = z1 - z2
        dw = w1 - w2
    in sqrt (dx * dx + dy * dy + dz * dz + dw * dw)
  norm (V4 x y z w) = sqrt (x * x + y * y + z * z + w * w)
  signorm v@(V4 x y z w) =
    let n = sqrt (x * x + y * y + z * z + w * w)
    in if n == 0.0 then v else V4 (x / n) (y / n) (z / n) (w / n)

-- | Convert a 3D vector to homogeneous coordinates with w=0.
-- |
-- | This represents a direction (not a point) in homogeneous coordinates.
-- |
-- | ```purescript
-- | vector (V3 1.0 0.0 0.0) = V4 1.0 0.0 0.0 0.0
-- | ```
vector :: forall a. Semiring a => V3 a -> V4 a
vector (V3 x y z) = V4 x y z zero

-- | Convert a 3D point to homogeneous coordinates with w=1.
-- |
-- | This represents a point (not a direction) in homogeneous coordinates.
-- |
-- | ```purescript
-- | point (V3 1.0 2.0 3.0) = V4 1.0 2.0 3.0 1.0
-- | ```
point :: forall a. Semiring a => V3 a -> V4 a
point (V3 x y z) = V4 x y z one

-- | Convert homogeneous coordinates back to 3D by dividing by w.
-- |
-- | ```purescript
-- | normalizePoint (V4 2.0 4.0 6.0 2.0) = V3 1.0 2.0 3.0
-- | ```
normalizePoint :: V4 Number -> V3 Number
normalizePoint (V4 x y z w) = V3 (x / w) (y / w) (z / w)

-- | Unit vector along the x-axis.
ex :: forall a. Semiring a => V4 a
ex = V4 one zero zero zero

-- | Unit vector along the y-axis.
ey :: forall a. Semiring a => V4 a
ey = V4 zero one zero zero

-- | Unit vector along the z-axis.
ez :: forall a. Semiring a => V4 a
ez = V4 zero zero one zero

-- | Unit vector along the w-axis.
ew :: forall a. Semiring a => V4 a
ew = V4 zero zero zero one

-- | Get the x component of a V4.
getX :: forall a. V4 a -> a
getX (V4 x _ _ _) = x

-- | Get the y component of a V4.
getY :: forall a. V4 a -> a
getY (V4 _ y _ _) = y

-- | Get the z component of a V4.
getZ :: forall a. V4 a -> a
getZ (V4 _ _ z _) = z

-- | Get the w component of a V4.
getW :: forall a. V4 a -> a
getW (V4 _ _ _ w) = w
