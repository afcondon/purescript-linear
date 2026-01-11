-- | Quaternions for 3D rotations.
-- |
-- | This module provides quaternions, which are the most efficient and
-- | numerically stable way to represent 3D rotations. Quaternions avoid
-- | gimbal lock and interpolate smoothly.
module Linear.Quaternion
  ( Quaternion(..)
  -- * Construction
  , axisAngle
  , fromEuler
  -- * Operations
  , rotate
  , conjugate
  , inverse
  , slerp
  -- * Accessors
  , getW
  , getXYZ
  , getX
  , getY
  , getZ
  -- * Quaternion algebra
  , qmul
  ) where

import Prelude

import Data.Foldable (class Foldable)
import Data.Traversable (class Traversable)
import Linear.Epsilon (class Epsilon, nearZero)
import Linear.Metric (class Metric, dot, norm, quadrance)
import Linear.V3 (V3(..), cross)
import Linear.Vector (class Additive, add, sub, lerp, liftU2, liftI2, (^*), (*^))
import Data.Number (sqrt, sin, cos, acos)

-- | A quaternion representing a rotation in 3D space.
-- |
-- | A quaternion q = w + xi + yj + zk is stored as `Quaternion w (V3 x y z)`.
-- | Unit quaternions (where |q| = 1) represent rotations.
-- |
-- | ```purescript
-- | identity = Quaternion 1.0 (V3 0.0 0.0 0.0)
-- | ```
data Quaternion a = Quaternion a (V3 a)

derive instance Eq a => Eq (Quaternion a)

instance Show a => Show (Quaternion a) where
  show (Quaternion w (V3 x y z)) =
    "(Quaternion " <> show w <> " " <> show x <> " " <> show y <> " " <> show z <> ")"

instance Functor Quaternion where
  map f (Quaternion w v) = Quaternion (f w) (map f v)

instance Apply Quaternion where
  apply (Quaternion fw fv) (Quaternion w v) = Quaternion (fw w) (apply fv v)

instance Applicative Quaternion where
  pure a = Quaternion a (pure a)

instance Foldable Quaternion where
  foldr f z (Quaternion w (V3 x y z')) = f w (f x (f y (f z' z)))
  foldl f z (Quaternion w (V3 x y z')) = f (f (f (f z w) x) y) z'
  foldMap f (Quaternion w (V3 x y z)) = f w <> f x <> f y <> f z

instance Traversable Quaternion where
  traverse f (Quaternion w (V3 x y z)) =
    Quaternion <$> f w <*> (V3 <$> f x <*> f y <*> f z)
  sequence (Quaternion mw (V3 mx my mz)) =
    Quaternion <$> mw <*> (V3 <$> mx <*> my <*> mz)

instance Additive Quaternion where
  zero = Quaternion zero (V3 zero zero zero)
  add (Quaternion w1 v1) (Quaternion w2 v2) = Quaternion (w1 + w2) (add v1 v2)
  sub (Quaternion w1 v1) (Quaternion w2 v2) = Quaternion (w1 - w2) (sub v1 v2)
  lerp t (Quaternion w1 v1) (Quaternion w2 v2) =
    Quaternion (w1 + t * (w2 - w1)) (lerp t v1 v2)
  liftU2 f (Quaternion w1 v1) (Quaternion w2 v2) = Quaternion (f w1 w2) (liftU2 f v1 v2)
  liftI2 f (Quaternion w1 v1) (Quaternion w2 v2) = Quaternion (f w1 w2) (liftI2 f v1 v2)

instance Metric Quaternion where
  dot (Quaternion w1 (V3 x1 y1 z1)) (Quaternion w2 (V3 x2 y2 z2)) =
    w1 * w2 + x1 * x2 + y1 * y2 + z1 * z2
  quadrance (Quaternion w (V3 x y z)) = w * w + x * x + y * y + z * z
  qd (Quaternion w1 (V3 x1 y1 z1)) (Quaternion w2 (V3 x2 y2 z2)) =
    let dw = w1 - w2
        dx = x1 - x2
        dy = y1 - y2
        dz = z1 - z2
    in dw * dw + dx * dx + dy * dy + dz * dz
  distance q1 q2 =
    let Quaternion w1 (V3 x1 y1 z1) = q1
        Quaternion w2 (V3 x2 y2 z2) = q2
        dw = w1 - w2
        dx = x1 - x2
        dy = y1 - y2
        dz = z1 - z2
    in sqrt (dw * dw + dx * dx + dy * dy + dz * dz)
  norm (Quaternion w (V3 x y z)) = sqrt (w * w + x * x + y * y + z * z)
  signorm q@(Quaternion w (V3 x y z)) =
    let n = sqrt (w * w + x * x + y * y + z * z)
    in if n == 0.0 then q else Quaternion (w / n) (V3 (x / n) (y / n) (z / n))

instance Semiring a => Semigroup (Quaternion a) where
  append (Quaternion w1 v1) (Quaternion w2 v2) = Quaternion (w1 + w2) (add v1 v2)

instance Semiring a => Monoid (Quaternion a) where
  mempty = Quaternion zero (V3 zero zero zero)

instance Epsilon a => Epsilon (Quaternion a) where
  nearZero (Quaternion w v) = nearZero w && nearZero v

-- | Quaternion multiplication (Hamilton product).
-- |
-- | This is NOT commutative: `qmul q1 q2 /= qmul q2 q1` in general.
-- | Composing rotations: to apply rotation q1 then q2, use `qmul q2 q1`.
-- |
-- | ```purescript
-- | qmul identity q = q
-- | qmul q identity = q
-- | ```
qmul :: forall a. Ring a => Quaternion a -> Quaternion a -> Quaternion a
qmul (Quaternion w1 (V3 x1 y1 z1)) (Quaternion w2 (V3 x2 y2 z2)) =
  Quaternion
    (w1 * w2 - (x1 * x2 + y1 * y2 + z1 * z2))
    (V3
      (w1 * x2 + x1 * w2 + y1 * z2 - z1 * y2)
      (w1 * y2 - x1 * z2 + y1 * w2 + z1 * x2)
      (w1 * z2 + x1 * y2 - y1 * x2 + z1 * w2))

-- | Construct a rotation quaternion from an axis and angle.
-- |
-- | The axis should be a unit vector. The angle is in radians.
-- |
-- | ```purescript
-- | -- 90 degree rotation around the Y axis
-- | axisAngle (V3 0.0 1.0 0.0) (pi / 2.0)
-- | ```
axisAngle :: V3 Number -> Number -> Quaternion Number
axisAngle axis theta =
  let halfAngle = theta / 2.0
      s = sin halfAngle
      c = cos halfAngle
  in Quaternion c (axis ^* s)

-- | Construct a rotation from Euler angles (in radians).
-- |
-- | Order is ZYX (yaw, pitch, roll).
-- |
-- | ```purescript
-- | fromEuler 0.0 0.0 0.0 = identity
-- | ```
fromEuler :: Number -> Number -> Number -> Quaternion Number
fromEuler yaw pitch roll =
  let cy = cos (yaw / 2.0)
      sy = sin (yaw / 2.0)
      cp = cos (pitch / 2.0)
      sp = sin (pitch / 2.0)
      cr = cos (roll / 2.0)
      sr = sin (roll / 2.0)
  in Quaternion
       (cr * cp * cy + sr * sp * sy)
       (V3
         (sr * cp * cy - cr * sp * sy)
         (cr * sp * cy + sr * cp * sy)
         (cr * cp * sy - sr * sp * cy))

-- | Rotate a 3D vector by a quaternion.
-- |
-- | The quaternion should be a unit quaternion for proper rotation.
-- |
-- | ```purescript
-- | let q = axisAngle (V3 0.0 1.0 0.0) (pi / 2.0)  -- 90° around Y
-- | rotate q (V3 1.0 0.0 0.0) ≈ V3 0.0 0.0 (-1.0)
-- | ```
rotate :: Quaternion Number -> V3 Number -> V3 Number
rotate q@(Quaternion w v) p =
  let -- The rotation formula: q * p * q^(-1)
      -- Optimized version using: p' = p + 2w(v × p) + 2(v × (v × p))
      t = 2.0 *^ cross v p
  in p `add` (w *^ t) `add` cross v t

-- | The conjugate of a quaternion.
-- |
-- | For unit quaternions, the conjugate equals the inverse and
-- | represents the opposite rotation.
-- |
-- | ```purescript
-- | conjugate (Quaternion w (V3 x y z)) = Quaternion w (V3 (-x) (-y) (-z))
-- | ```
conjugate :: forall a. Ring a => Quaternion a -> Quaternion a
conjugate (Quaternion w v) = Quaternion w (map negate v)

-- | The multiplicative inverse of a quaternion.
-- |
-- | For unit quaternions, this equals the conjugate.
-- |
-- | ```purescript
-- | qmul q (inverse q) ≈ identity
-- | ```
inverse :: Quaternion Number -> Quaternion Number
inverse (Quaternion w (V3 x y z)) =
  let n2 = w * w + x * x + y * y + z * z
  in Quaternion (w / n2) (V3 (negate x / n2) (negate y / n2) (negate z / n2))

-- | Spherical linear interpolation between two quaternions.
-- |
-- | This produces smooth rotation interpolation along the shortest path.
-- |
-- | ```purescript
-- | slerp 0.0 q1 q2 = q1
-- | slerp 1.0 q1 q2 = q2
-- | slerp 0.5 q1 q2 -- halfway rotation
-- | ```
slerp :: Number -> Quaternion Number -> Quaternion Number -> Quaternion Number
slerp t q1 q2' =
  let -- Ensure we take the shortest path
      d = qdot q1 q2'
      q2 = if d < 0.0 then map negate q2' else q2'
      d' = if d < 0.0 then negate d else d
  in if d' > 0.9995
     -- Linear interpolation for nearly identical quaternions
     then qsignorm (qlerp t q1 q2)
     else
       let theta = acos d'
           thetaT = theta * t
           q3 = qsignorm (qsub q2 (q1 ^* d'))
       in (q1 ^* cos thetaT) `qadd` (q3 ^* sin thetaT)
  where
  -- Local helpers to avoid circular dependencies
  qdot (Quaternion w1 (V3 x1 y1 z1)) (Quaternion w2 (V3 x2 y2 z2)) =
    w1 * w2 + x1 * x2 + y1 * y2 + z1 * z2
  qlerp t' (Quaternion w1 (V3 x1 y1 z1)) (Quaternion w2 (V3 x2 y2 z2)) =
    Quaternion
      (w1 + t' * (w2 - w1))
      (V3 (x1 + t' * (x2 - x1)) (y1 + t' * (y2 - y1)) (z1 + t' * (z2 - z1)))
  qadd (Quaternion w1 (V3 x1 y1 z1)) (Quaternion w2 (V3 x2 y2 z2)) =
    Quaternion (w1 + w2) (V3 (x1 + x2) (y1 + y2) (z1 + z2))
  qsub (Quaternion w1 (V3 x1 y1 z1)) (Quaternion w2 (V3 x2 y2 z2)) =
    Quaternion (w1 - w2) (V3 (x1 - x2) (y1 - y2) (z1 - z2))
  qsignorm q@(Quaternion w (V3 x y z)) =
    let n = sqrt (w * w + x * x + y * y + z * z)
    in if n == 0.0 then q else Quaternion (w / n) (V3 (x / n) (y / n) (z / n))

-- | Get the scalar (w) component.
getW :: forall a. Quaternion a -> a
getW (Quaternion w _) = w

-- | Get the vector (xyz) components.
getXYZ :: forall a. Quaternion a -> V3 a
getXYZ (Quaternion _ v) = v

-- | Get the x component.
getX :: forall a. Quaternion a -> a
getX (Quaternion _ (V3 x _ _)) = x

-- | Get the y component.
getY :: forall a. Quaternion a -> a
getY (Quaternion _ (V3 _ y _)) = y

-- | Get the z component.
getZ :: forall a. Quaternion a -> a
getZ (Quaternion _ (V3 _ _ z)) = z
