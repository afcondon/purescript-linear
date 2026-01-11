-- | Property tests for Quaternion operations.
module Test.Linear.Quaternion where

import Prelude

import Data.Number (abs, pi, sqrt)
import Effect (Effect)
import Effect.Console (log)
import Linear.Quaternion (Quaternion(..), axisAngle, rotate, conjugate, qmul, slerp)
import Linear.V3 (V3(..))
import Linear.Metric (norm, quadrance)
import Test.Linear.Arbitrary (UnitQuaternion(..), NonZeroV3(..), approxEq, approxEqLoose)
import Test.QuickCheck (Result, quickCheck, (<?>))
import Test.QuickCheck.Gen (choose)
import Test.QuickCheck (class Arbitrary, arbitrary)

-- | All Quaternion tests
runQuaternionTests :: Effect Unit
runQuaternionTests = do
  log "Testing Quaternion rotation properties..."
  testRotation
  log "Testing Quaternion algebra..."
  testAlgebra
  log "Testing slerp..."
  testSlerp

-- Rotation tests
testRotation :: Effect Unit
testRotation = do
  log "  rotation preserves vector length"
  quickCheck propRotationPreservesLength
  log "  identity rotation (angle=0) leaves vector unchanged"
  quickCheck propIdentityRotation
  log "  double rotation by pi equals identity"
  quickCheck propDoubleRotation
  log "  conjugate reverses rotation"
  quickCheck propConjugateReversesRotation

propRotationPreservesLength :: UnitQuaternion -> NonZeroV3 -> Result
propRotationPreservesLength (UnitQuaternion q) (NonZeroV3 v) =
  let rotated = rotate q v
      originalNorm = norm v
      rotatedNorm = norm rotated
  in approxEqLoose originalNorm rotatedNorm
     <?> "rotation should preserve vector length"

propIdentityRotation :: NonZeroV3 -> NonZeroV3 -> Result
propIdentityRotation (NonZeroV3 axis) (NonZeroV3 v) =
  let unitAxis = signormV3 axis
      q = axisAngle unitAxis 0.0  -- zero rotation
      rotated = rotate q v
  in v3EqLoose rotated v <?> "zero rotation should leave vector unchanged"

propDoubleRotation :: NonZeroV3 -> NonZeroV3 -> Result
propDoubleRotation (NonZeroV3 axis) (NonZeroV3 v) =
  let unitAxis = signormV3 axis
      q = axisAngle unitAxis pi  -- 180 degree rotation
      rotatedOnce = rotate q v
      rotatedTwice = rotate q rotatedOnce
  in v3EqLoose rotatedTwice v <?> "rotating by pi twice should return to original"

propConjugateReversesRotation :: UnitQuaternion -> NonZeroV3 -> Result
propConjugateReversesRotation (UnitQuaternion q) (NonZeroV3 v) =
  let rotated = rotate q v
      qConj = conjugate q
      unrotated = rotate qConj rotated
  in v3EqLoose unrotated v <?> "conjugate should reverse rotation"

-- Algebra tests
testAlgebra :: Effect Unit
testAlgebra = do
  log "  qmul is associative: (p * q) * r = p * (q * r)"
  quickCheck propQmulAssociative
  log "  identity quaternion: qmul identity q = q"
  quickCheck propQmulIdentity
  log "  composed rotations: rotate (q2 * q1) v = rotate q2 (rotate q1 v)"
  quickCheck propComposedRotation

propQmulAssociative :: UnitQuaternion -> UnitQuaternion -> UnitQuaternion -> Result
propQmulAssociative (UnitQuaternion p) (UnitQuaternion q) (UnitQuaternion r) =
  let left = qmul (qmul p q) r
      right = qmul p (qmul q r)
  in qEqLoose left right <?> "qmul should be associative"

propQmulIdentity :: UnitQuaternion -> Result
propQmulIdentity (UnitQuaternion q) =
  let identity = Quaternion 1.0 (V3 0.0 0.0 0.0)
      result = qmul identity q
  in qEqLoose result q <?> "identity qmul should leave quaternion unchanged"

propComposedRotation :: UnitQuaternion -> UnitQuaternion -> NonZeroV3 -> Result
propComposedRotation (UnitQuaternion q1) (UnitQuaternion q2) (NonZeroV3 v) =
  let -- Apply q1 then q2 as separate rotations
      step1 = rotate q1 v
      step2 = rotate q2 step1
      -- Apply composed rotation (q2 * q1 because rotation is right-to-left)
      composed = qmul q2 q1
      direct = rotate composed v
  in v3EqLoose step2 direct <?> "composed rotation should equal sequential rotations"

-- Slerp tests
testSlerp :: Effect Unit
testSlerp = do
  log "  slerp at t=0 returns first quaternion"
  quickCheck propSlerpZero
  log "  slerp at t=1 returns second quaternion"
  quickCheck propSlerpOne
  log "  slerp produces unit quaternions"
  quickCheck propSlerpUnit

propSlerpZero :: UnitQuaternion -> UnitQuaternion -> Result
propSlerpZero (UnitQuaternion q1) (UnitQuaternion q2) =
  let result = slerp 0.0 q1 q2
  in qEqLoose result q1 <?> "slerp 0 should return first quaternion"

propSlerpOne :: UnitQuaternion -> UnitQuaternion -> Result
propSlerpOne (UnitQuaternion q1) (UnitQuaternion q2) =
  let result = slerp 1.0 q1 q2
      -- Note: result might be negated (same rotation) so check both
  in (qEqLoose result q2 || qEqLoose result (negateQ q2))
     <?> "slerp 1 should return second quaternion (or its negative)"

-- | Test that slerp produces unit quaternions
newtype SlerpT = SlerpT Number

instance Arbitrary SlerpT where
  arbitrary = SlerpT <$> choose 0.0 1.0

propSlerpUnit :: UnitQuaternion -> UnitQuaternion -> SlerpT -> Result
propSlerpUnit (UnitQuaternion q1) (UnitQuaternion q2) (SlerpT t) =
  let result = slerp t q1 q2
      n = qNorm result
  in approxEqLoose n 1.0 <?> "slerp should produce unit quaternion"

-- Helper functions
signormV3 :: V3 Number -> V3 Number
signormV3 v@(V3 x y z) =
  let n = norm v
  in if n == 0.0 then v else V3 (x / n) (y / n) (z / n)

v3EqLoose :: V3 Number -> V3 Number -> Boolean
v3EqLoose (V3 x1 y1 z1) (V3 x2 y2 z2) =
  approxEqLoose x1 x2 && approxEqLoose y1 y2 && approxEqLoose z1 z2

qEqLoose :: Quaternion Number -> Quaternion Number -> Boolean
qEqLoose (Quaternion w1 (V3 x1 y1 z1)) (Quaternion w2 (V3 x2 y2 z2)) =
  approxEqLoose w1 w2 && approxEqLoose x1 x2 && approxEqLoose y1 y2 && approxEqLoose z1 z2

negateQ :: Quaternion Number -> Quaternion Number
negateQ (Quaternion w (V3 x y z)) = Quaternion (negate w) (V3 (negate x) (negate y) (negate z))

qNorm :: Quaternion Number -> Number
qNorm (Quaternion w (V3 x y z)) = sqrt (w * w + x * x + y * y + z * z)
