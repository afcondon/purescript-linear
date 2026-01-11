-- | 2-dimensional vectors.
-- |
-- | This module provides the `V2` type for 2D vector operations,
-- | commonly used for 2D graphics, physics, and game development.
module Linear.V2
  ( V2(..)
  -- * Geometric functions
  , perp
  , angle
  , unangle
  , crossZ
  -- * Basis vectors
  , ex
  , ey
  -- * Accessors
  , getX
  , getY
  ) where

import Prelude

import Data.Foldable (class Foldable)
import Data.Traversable (class Traversable)
import Linear.Epsilon (class Epsilon, nearZero)
import Linear.Metric (class Metric)
import Linear.Vector (class Additive, (^/))
import Data.Number (sqrt, cos, sin, atan2)

-- | A 2-dimensional vector.
-- |
-- | ```purescript
-- | origin = V2 0.0 0.0
-- | direction = V2 1.0 0.0
-- | ```
data V2 a = V2 a a

derive instance Eq a => Eq (V2 a)
derive instance Ord a => Ord (V2 a)

instance Show a => Show (V2 a) where
  show (V2 x y) = "(V2 " <> show x <> " " <> show y <> ")"

instance Functor V2 where
  map f (V2 x y) = V2 (f x) (f y)

instance Apply V2 where
  apply (V2 fx fy) (V2 x y) = V2 (fx x) (fy y)

instance Applicative V2 where
  pure a = V2 a a

instance Bind V2 where
  bind (V2 x y) f = V2 x' y'
    where
    V2 x' _ = f x
    V2 _ y' = f y

instance Monad V2

instance Foldable V2 where
  foldr f z (V2 x y) = f x (f y z)
  foldl f z (V2 x y) = f (f z x) y
  foldMap f (V2 x y) = f x <> f y

instance Traversable V2 where
  traverse f (V2 x y) = V2 <$> f x <*> f y
  sequence (V2 mx my) = V2 <$> mx <*> my

instance Additive V2 where
  zero = V2 zero zero
  add (V2 x1 y1) (V2 x2 y2) = V2 (x1 + x2) (y1 + y2)
  sub (V2 x1 y1) (V2 x2 y2) = V2 (x1 - x2) (y1 - y2)
  lerp t (V2 x1 y1) (V2 x2 y2) = V2 (x1 + t * (x2 - x1)) (y1 + t * (y2 - y1))
  liftU2 f (V2 x1 y1) (V2 x2 y2) = V2 (f x1 x2) (f y1 y2)
  liftI2 f (V2 x1 y1) (V2 x2 y2) = V2 (f x1 x2) (f y1 y2)

instance Semiring a => Semigroup (V2 a) where
  append (V2 x1 y1) (V2 x2 y2) = V2 (x1 + x2) (y1 + y2)

instance Semiring a => Monoid (V2 a) where
  mempty = V2 zero zero

instance Epsilon a => Epsilon (V2 a) where
  nearZero (V2 x y) = nearZero x && nearZero y

instance Metric V2 where
  dot (V2 x1 y1) (V2 x2 y2) = x1 * x2 + y1 * y2
  quadrance (V2 x y) = x * x + y * y
  qd (V2 x1 y1) (V2 x2 y2) =
    let dx = x1 - x2
        dy = y1 - y2
    in dx * dx + dy * dy
  distance v1 v2 =
    let V2 x1 y1 = v1
        V2 x2 y2 = v2
        dx = x1 - x2
        dy = y1 - y2
    in sqrt (dx * dx + dy * dy)
  norm (V2 x y) = sqrt (x * x + y * y)
  signorm v@(V2 x y) =
    let n = sqrt (x * x + y * y)
    in if n == 0.0 then v else V2 (x / n) (y / n)

-- | The counter-clockwise perpendicular vector.
-- |
-- | ```purescript
-- | perp (V2 1.0 0.0) = V2 0.0 1.0
-- | perp (V2 0.0 1.0) = V2 (-1.0) 0.0
-- | ```
perp :: forall a. Ring a => V2 a -> V2 a
perp (V2 x y) = V2 (negate y) x

-- | Construct a unit vector at the given angle (in radians) from the positive x-axis.
-- |
-- | ```purescript
-- | angle 0.0 = V2 1.0 0.0
-- | angle (pi / 2.0) ≈ V2 0.0 1.0
-- | ```
angle :: Number -> V2 Number
angle theta = V2 (cos theta) (sin theta)

-- | Extract the angle (in radians) from a vector, measured from the positive x-axis.
-- |
-- | ```purescript
-- | unangle (V2 1.0 0.0) = 0.0
-- | unangle (V2 0.0 1.0) = pi / 2.0
-- | ```
unangle :: V2 Number -> Number
unangle (V2 x y) = atan2 y x

-- | The z-component of the cross product of two 2D vectors.
-- |
-- | This is useful for determining the orientation of two vectors:
-- | - Positive: v2 is counter-clockwise from v1
-- | - Negative: v2 is clockwise from v1
-- | - Zero: vectors are parallel
-- |
-- | ```purescript
-- | crossZ (V2 1.0 0.0) (V2 0.0 1.0) = 1.0  -- counter-clockwise
-- | crossZ (V2 0.0 1.0) (V2 1.0 0.0) = -1.0 -- clockwise
-- | ```
crossZ :: forall a. Ring a => V2 a -> V2 a -> a
crossZ (V2 x1 y1) (V2 x2 y2) = x1 * y2 - y1 * x2

-- | Unit vector along the x-axis.
ex :: forall a. Semiring a => V2 a
ex = V2 one zero

-- | Unit vector along the y-axis.
ey :: forall a. Semiring a => V2 a
ey = V2 zero one

-- | Get the x component of a V2.
getX :: forall a. V2 a -> a
getX (V2 x _) = x

-- | Get the y component of a V2.
getY :: forall a. V2 a -> a
getY (V2 _ y) = y
