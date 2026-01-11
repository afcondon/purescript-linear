-- | Matrix types and operations.
-- |
-- | Matrices are represented as vectors of row vectors, following Haskell's linear.
-- | This module provides type aliases and operations for common matrix sizes.
module Linear.Matrix
  ( -- * Type aliases
    M22
  , M23
  , M24
  , M32
  , M33
  , M34
  , M42
  , M43
  , M44
  -- * Matrix construction
  , identity22
  , identity33
  , identity44
  , fromQuaternion
  , mkTransformation
  , mkTransformationMat
  -- * Matrix operations
  , mulMV
  , mulVM
  , mulMM
  , transpose22
  , transpose33
  , transpose44
  , (!*)
  , (*!)
  , (!*!)
  -- * Determinant and inverse
  , det22
  , det33
  , det44
  , inv22
  , inv33
  , inv44
  -- * Diagonal and trace
  , diagonal33
  , trace33
  ) where

import Prelude

import Data.Maybe (Maybe(..))
import Linear.Quaternion (Quaternion(..))
import Linear.V2 (V2(..))
import Linear.V3 (V3(..))
import Linear.V4 (V4(..))

-- | 2x2 matrix (2 rows, 2 columns)
type M22 a = V2 (V2 a)

-- | 2x3 matrix
type M23 a = V2 (V3 a)

-- | 2x4 matrix
type M24 a = V2 (V4 a)

-- | 3x2 matrix
type M32 a = V3 (V2 a)

-- | 3x3 matrix
type M33 a = V3 (V3 a)

-- | 3x4 matrix
type M34 a = V3 (V4 a)

-- | 4x2 matrix
type M42 a = V4 (V2 a)

-- | 4x3 matrix
type M43 a = V4 (V3 a)

-- | 4x4 matrix (commonly used for 3D transformations)
type M44 a = V4 (V4 a)

-- | 2x2 identity matrix.
identity22 :: forall a. Semiring a => M22 a
identity22 = V2
  (V2 one zero)
  (V2 zero one)

-- | 3x3 identity matrix.
identity33 :: forall a. Semiring a => M33 a
identity33 = V3
  (V3 one zero zero)
  (V3 zero one zero)
  (V3 zero zero one)

-- | 4x4 identity matrix.
identity44 :: forall a. Semiring a => M44 a
identity44 = V4
  (V4 one zero zero zero)
  (V4 zero one zero zero)
  (V4 zero zero one zero)
  (V4 zero zero zero one)

-- | Multiply a matrix by a column vector (M * v).
-- |
-- | ```purescript
-- | mulMV identity33 v = v
-- | ```
mulMV :: forall a. Semiring a => M33 a -> V3 a -> V3 a
mulMV (V3 r0 r1 r2) v = V3 (dot3 r0 v) (dot3 r1 v) (dot3 r2 v)
  where
  dot3 (V3 x1 y1 z1) (V3 x2 y2 z2) = x1 * x2 + y1 * y2 + z1 * z2

-- | Multiply a row vector by a matrix (v * M).
mulVM :: forall a. Semiring a => V3 a -> M33 a -> V3 a
mulVM v m = mulMV (transpose33 m) v

infixl 7 mulMV as !*
infixl 7 mulVM as *!

-- | Matrix multiplication.
-- |
-- | ```purescript
-- | mulMM identity33 m = m
-- | mulMM m identity33 = m
-- | ```
mulMM :: forall a. Semiring a => M33 a -> M33 a -> M33 a
mulMM m1 m2 =
  let m2t = transpose33 m2
  in map (\row -> map (dot3 row) m2t) m1
  where
  dot3 (V3 x1 y1 z1) (V3 x2 y2 z2) = x1 * x2 + y1 * y2 + z1 * z2

infixl 7 mulMM as !*!

-- | Transpose a 2x2 matrix.
transpose22 :: forall a. M22 a -> M22 a
transpose22 (V2 (V2 a b) (V2 c d)) =
  V2 (V2 a c) (V2 b d)

-- | Transpose a 3x3 matrix.
transpose33 :: forall a. M33 a -> M33 a
transpose33 (V3 (V3 a b c) (V3 d e f) (V3 g h i)) =
  V3 (V3 a d g) (V3 b e h) (V3 c f i)

-- | Transpose a 4x4 matrix.
transpose44 :: forall a. M44 a -> M44 a
transpose44 (V4 (V4 a b c d) (V4 e f g h) (V4 i j k l) (V4 m n o p)) =
  V4 (V4 a e i m) (V4 b f j n) (V4 c g k o) (V4 d h l p)

-- | Determinant of a 2x2 matrix.
det22 :: forall a. Ring a => M22 a -> a
det22 (V2 (V2 a b) (V2 c d)) = a * d - b * c

-- | Determinant of a 3x3 matrix.
det33 :: forall a. Ring a => M33 a -> a
det33 (V3 (V3 a b c) (V3 d e f) (V3 g h i)) =
  a * (e * i - f * h) - b * (d * i - f * g) + c * (d * h - e * g)

-- | Determinant of a 4x4 matrix.
det44 :: forall a. Ring a => M44 a -> a
det44 (V4 (V4 a b c d) (V4 e f g h) (V4 i j k l) (V4 m n o p)) =
  let s0 = a * f - e * b
      s1 = a * g - i * b
      s2 = a * h - m * b
      s3 = e * g - i * f
      s4 = e * h - m * f
      s5 = i * h - m * g
      c5 = k * p - o * l
      c4 = j * p - n * l
      c3 = j * o - n * k
      c2 = c * p - o * d
      c1 = c * n - j * d
      c0 = c * l - k * d
  in s0 * c5 - s1 * c4 + s2 * c3 + s3 * c2 - s4 * c1 + s5 * c0

-- | Inverse of a 2x2 matrix, if it exists.
inv22 :: M22 Number -> Maybe (M22 Number)
inv22 m@(V2 (V2 a b) (V2 c d)) =
  let d' = det22 m
  in if d' == 0.0
     then Nothing
     else Just $ V2
       (V2 (d / d') (negate b / d'))
       (V2 (negate c / d') (a / d'))

-- | Inverse of a 3x3 matrix, if it exists.
inv33 :: M33 Number -> Maybe (M33 Number)
inv33 m@(V3 (V3 a b c) (V3 d e f) (V3 g h i)) =
  let det = det33 m
  in if det == 0.0
     then Nothing
     else
       let invDet = 1.0 / det
           a' = (e * i - f * h) * invDet
           b' = (c * h - b * i) * invDet
           c' = (b * f - c * e) * invDet
           d' = (f * g - d * i) * invDet
           e' = (a * i - c * g) * invDet
           f' = (c * d - a * f) * invDet
           g' = (d * h - e * g) * invDet
           h' = (b * g - a * h) * invDet
           i' = (a * e - b * d) * invDet
       in Just $ V3 (V3 a' b' c') (V3 d' e' f') (V3 g' h' i')

-- | Inverse of a 4x4 matrix, if it exists.
inv44 :: M44 Number -> Maybe (M44 Number)
inv44 (V4 (V4 a b c d) (V4 e f g h) (V4 i j k l) (V4 m n o p)) =
  let s0 = a * f - e * b
      s1 = a * g - i * b
      s2 = a * h - m * b
      s3 = e * g - i * f
      s4 = e * h - m * f
      s5 = i * h - m * g
      c5 = k * p - o * l
      c4 = j * p - n * l
      c3 = j * o - n * k
      c2 = c * p - o * d
      c1 = c * n - j * d
      c0 = c * l - k * d
      det' = s0 * c5 - s1 * c4 + s2 * c3 + s3 * c2 - s4 * c1 + s5 * c0
  in if det' == 0.0
     then Nothing
     else
       let invDet = 1.0 / det'
       in Just $ V4
         (V4
           ((f * c5 - g * c4 + h * c3) * invDet)
           ((negate b * c5 + c * c4 - d * c3) * invDet)
           ((n * s5 - o * s4 + p * s3) * invDet)
           ((negate j * s5 + k * s4 - l * s3) * invDet))
         (V4
           ((negate e * c5 + g * c2 - h * c1) * invDet)
           ((a * c5 - c * c2 + d * c1) * invDet)
           ((negate m * s5 + o * s2 - p * s1) * invDet)
           ((i * s5 - k * s2 + l * s1) * invDet))
         (V4
           ((e * c4 - f * c2 + h * c0) * invDet)
           ((negate a * c4 + b * c2 - d * c0) * invDet)
           ((m * s4 - n * s2 + p * s0) * invDet)
           ((negate i * s4 + j * s2 - l * s0) * invDet))
         (V4
           ((negate e * c3 + f * c1 - g * c0) * invDet)
           ((a * c3 - b * c1 + c * c0) * invDet)
           ((negate m * s3 + n * s1 - o * s0) * invDet)
           ((i * s3 - j * s1 + k * s0) * invDet))

-- | Extract the diagonal of a 3x3 matrix.
diagonal33 :: forall a. M33 a -> V3 a
diagonal33 (V3 (V3 a _ _) (V3 _ b _) (V3 _ _ c)) = V3 a b c

-- | Compute the trace (sum of diagonal) of a 3x3 matrix.
trace33 :: forall a. Semiring a => M33 a -> a
trace33 (V3 (V3 a _ _) (V3 _ b _) (V3 _ _ c)) = a + b + c

-- | Convert a unit quaternion to a 3x3 rotation matrix.
-- |
-- | ```purescript
-- | fromQuaternion (Quaternion 1.0 (V3 0.0 0.0 0.0)) = identity33
-- | ```
fromQuaternion :: Quaternion Number -> M33 Number
fromQuaternion (Quaternion w (V3 x y z)) =
  let xx = x * x
      yy = y * y
      zz = z * z
      xy = x * y
      xz = x * z
      yz = y * z
      wx = w * x
      wy = w * y
      wz = w * z
  in V3
    (V3 (1.0 - 2.0 * (yy + zz)) (2.0 * (xy - wz)) (2.0 * (xz + wy)))
    (V3 (2.0 * (xy + wz)) (1.0 - 2.0 * (xx + zz)) (2.0 * (yz - wx)))
    (V3 (2.0 * (xz - wy)) (2.0 * (yz + wx)) (1.0 - 2.0 * (xx + yy)))

-- | Build a 4x4 transformation matrix from a quaternion rotation and translation.
-- |
-- | The resulting matrix applies rotation first, then translation.
-- |
-- | ```purescript
-- | mkTransformation identity (V3 1.0 2.0 3.0) -- pure translation
-- | mkTransformation rotation (V3 0.0 0.0 0.0) -- pure rotation
-- | ```
mkTransformation :: Quaternion Number -> V3 Number -> M44 Number
mkTransformation q (V3 tx ty tz) =
  let V3 (V3 r00 r01 r02) (V3 r10 r11 r12) (V3 r20 r21 r22) = fromQuaternion q
  in V4
    (V4 r00 r01 r02 tx)
    (V4 r10 r11 r12 ty)
    (V4 r20 r21 r22 tz)
    (V4 0.0 0.0 0.0 1.0)

-- | Build a 4x4 transformation matrix from a 3x3 rotation matrix and translation.
mkTransformationMat :: M33 Number -> V3 Number -> M44 Number
mkTransformationMat (V3 (V3 r00 r01 r02) (V3 r10 r11 r12) (V3 r20 r21 r22)) (V3 tx ty tz) =
  V4
    (V4 r00 r01 r02 tx)
    (V4 r10 r11 r12 ty)
    (V4 r20 r21 r22 tz)
    (V4 0.0 0.0 0.0 1.0)
