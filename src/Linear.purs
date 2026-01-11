-- | Linear algebra for graphics programming.
-- |
-- | This module re-exports the core functionality from the linear package.
-- | It provides fixed-size vectors (V2, V3, V4), quaternions for 3D rotations,
-- | and matrix operations commonly used in graphics and game development.
-- |
-- | ## Quick Start
-- |
-- | ```purescript
-- | import Linear
-- |
-- | -- 2D vector operations
-- | v1 = V2 3.0 4.0
-- | v2 = V2 1.0 0.0
-- | sum = v1 ^+^ v2         -- V2 4.0 4.0
-- | length = norm v1        -- 5.0
-- | unit = signorm v1       -- V2 0.6 0.8
-- |
-- | -- 3D cross product
-- | x = V3 1.0 0.0 0.0
-- | y = V3 0.0 1.0 0.0
-- | z = cross x y           -- V3 0.0 0.0 1.0
-- |
-- | -- Quaternion rotation
-- | q = axisAngle (V3 0.0 1.0 0.0) (pi / 2.0)  -- 90° around Y
-- | rotated = rotate q (V3 1.0 0.0 0.0)         -- ≈ V3 0.0 0.0 (-1.0)
-- | ```
module Linear
  ( module Linear.Epsilon
  , module Linear.Vector
  , module Linear.Metric
  , module Linear.V2
  , module Linear.V3
  , module Linear.V4
  , module Linear.Affine
  , module Linear.Quaternion
  , module Linear.Matrix
  ) where

import Linear.Epsilon (class Epsilon, nearZero)
import Linear.Vector (class Additive, zero, add, sub, lerp, liftU2, liftI2, (^+^), (^-^), (^*), (*^), (^/), negated, sumV)
import Linear.Metric (class Metric, dot, quadrance, qd, distance, norm, signorm, normalize, project)
import Linear.V2 (V2(..), perp, angle, unangle, crossZ)
import Linear.V3 (V3(..), cross, triple)
import Linear.V4 (V4(..), vector, point, normalizePoint)
import Linear.Affine (Point(..), unP, class Affine, diff, moveBy, moveByNeg, (.-.), (.+^), (.-^), origin, distanceA, qdA, lerpP)
import Linear.Quaternion (Quaternion(..), axisAngle, fromEuler, rotate, conjugate, inverse, slerp, qmul)
import Linear.Matrix (M22, M23, M24, M32, M33, M34, M42, M43, M44, identity22, identity33, identity44, fromQuaternion, mkTransformation, mkTransformationMat, (!*), (*!), (!*!), transpose22, transpose33, transpose44, det22, det33, det44, inv22, inv33, inv44, diagonal33, trace33)
