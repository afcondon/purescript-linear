-- | Property tests for the Metric typeclass.
module Test.Linear.Metric where

import Prelude

import Effect (Effect)
import Effect.Console (log)
import Linear.V3 (V3(..), cross)
import Linear.Metric (dot, quadrance, qd, distance, norm, signorm)
import Linear.Vector ((^+^), (^-^), (^*))
import Test.Linear.Arbitrary (ArbV2(..), ArbV3(..), NonZeroV2(..), NonZeroV3(..), approxEq, approxEqLoose)
import Test.QuickCheck (Result, quickCheck, (<?>))

-- | All Metric tests
runMetricTests :: Effect Unit
runMetricTests = do
  log "Testing Metric laws for V2..."
  testMetricV2
  log "Testing Metric laws for V3..."
  testMetricV3
  log "Testing V3 cross product..."
  testCrossProduct

-- V2 Tests
testMetricV2 :: Effect Unit
testMetricV2 = do
  log "  quadrance = dot self: quadrance v = dot v v"
  quickCheck propQuadranceDotSelfV2
  log "  norm^2 = quadrance: norm v ^ 2 = quadrance v"
  quickCheck propNormQuadranceV2
  log "  dot is symmetric: dot u v = dot v u"
  quickCheck propDotSymmetricV2
  log "  dot is bilinear: dot (u ^+^ v) w = dot u w + dot v w"
  quickCheck propDotBilinearV2
  log "  signorm has unit length (non-zero input)"
  quickCheck propSignormUnitV2
  log "  distance is symmetric: distance u v = distance v u"
  quickCheck propDistanceSymmetricV2
  log "  qd = quadrance of difference"
  quickCheck propQdV2

propQuadranceDotSelfV2 :: ArbV2 -> Result
propQuadranceDotSelfV2 (ArbV2 v) =
  approxEq (quadrance v) (dot v v) <?> "quadrance v should equal dot v v"

propNormQuadranceV2 :: ArbV2 -> Result
propNormQuadranceV2 (ArbV2 v) =
  approxEq (norm v * norm v) (quadrance v) <?> "norm^2 should equal quadrance"

propDotSymmetricV2 :: ArbV2 -> ArbV2 -> Result
propDotSymmetricV2 (ArbV2 u) (ArbV2 v) =
  approxEq (dot u v) (dot v u) <?> "dot should be symmetric"

propDotBilinearV2 :: ArbV2 -> ArbV2 -> ArbV2 -> Result
propDotBilinearV2 (ArbV2 u) (ArbV2 v) (ArbV2 w) =
  approxEqLoose (dot (u ^+^ v) w) (dot u w + dot v w) <?> "dot should be bilinear"

propSignormUnitV2 :: NonZeroV2 -> Result
propSignormUnitV2 (NonZeroV2 v) =
  approxEq (norm (signorm v)) 1.0 <?> "signorm should produce unit vector"

propDistanceSymmetricV2 :: ArbV2 -> ArbV2 -> Result
propDistanceSymmetricV2 (ArbV2 u) (ArbV2 v) =
  approxEq (distance u v) (distance v u) <?> "distance should be symmetric"

propQdV2 :: ArbV2 -> ArbV2 -> Result
propQdV2 (ArbV2 u) (ArbV2 v) =
  approxEq (qd u v) (quadrance (u ^-^ v)) <?> "qd should equal quadrance of difference"

-- V3 Tests
testMetricV3 :: Effect Unit
testMetricV3 = do
  log "  quadrance = dot self"
  quickCheck propQuadranceDotSelfV3
  log "  norm^2 = quadrance"
  quickCheck propNormQuadranceV3
  log "  dot is symmetric"
  quickCheck propDotSymmetricV3
  log "  signorm has unit length (non-zero input)"
  quickCheck propSignormUnitV3
  log "  triangle inequality: norm (u + v) <= norm u + norm v"
  quickCheck propTriangleInequalityV3

propQuadranceDotSelfV3 :: ArbV3 -> Result
propQuadranceDotSelfV3 (ArbV3 v) =
  approxEq (quadrance v) (dot v v) <?> "quadrance = dot self"

propNormQuadranceV3 :: ArbV3 -> Result
propNormQuadranceV3 (ArbV3 v) =
  approxEq (norm v * norm v) (quadrance v) <?> "norm^2 = quadrance"

propDotSymmetricV3 :: ArbV3 -> ArbV3 -> Result
propDotSymmetricV3 (ArbV3 u) (ArbV3 v) =
  approxEq (dot u v) (dot v u) <?> "dot symmetric"

propSignormUnitV3 :: NonZeroV3 -> Result
propSignormUnitV3 (NonZeroV3 v) =
  approxEq (norm (signorm v)) 1.0 <?> "signorm unit"

propTriangleInequalityV3 :: ArbV3 -> ArbV3 -> Result
propTriangleInequalityV3 (ArbV3 u) (ArbV3 v) =
  let sumNorm = norm (u ^+^ v)
      normSum = norm u + norm v
  in (sumNorm <= normSum + 1.0e-6) <?> "triangle inequality"

-- Cross product tests
testCrossProduct :: Effect Unit
testCrossProduct = do
  log "  cross product is anti-commutative: u x v = -(v x u)"
  quickCheck propCrossAntiCommutative
  log "  cross product is perpendicular: dot (u x v) u = 0"
  quickCheck propCrossPerpendicular
  log "  cross of parallel vectors is zero"
  quickCheck propCrossParallel

propCrossAntiCommutative :: ArbV3 -> ArbV3 -> Result
propCrossAntiCommutative (ArbV3 u) (ArbV3 v) =
  let uv = cross u v
      vu = cross v u
      V3 x1 y1 z1 = uv
      V3 x2 y2 z2 = vu
  in approxEq x1 (negate x2) && approxEq y1 (negate y2) && approxEq z1 (negate z2)
     <?> "cross should be anti-commutative"

propCrossPerpendicular :: ArbV3 -> ArbV3 -> Result
propCrossPerpendicular (ArbV3 u) (ArbV3 v) =
  let c = cross u v
  in approxEqLoose (dot c u) 0.0 && approxEqLoose (dot c v) 0.0
     <?> "cross product should be perpendicular to both inputs"

propCrossParallel :: ArbV3 -> Number -> Result
propCrossParallel (ArbV3 v) s =
  let parallel = v ^* s
      c = cross v parallel
      V3 x y z = c
  in approxEqLoose x 0.0 && approxEqLoose y 0.0 && approxEqLoose z 0.0
     <?> "cross of parallel vectors should be zero"
