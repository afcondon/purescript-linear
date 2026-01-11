-- | Property tests for the Additive typeclass.
module Test.Linear.Additive where

import Prelude hiding (zero)

import Effect (Effect)
import Effect.Console (log)
import Linear.V2 (V2(..))
import Linear.V3 (V3(..))
import Linear.V4 (V4(..))
import Linear.Vector (zero, lerp, (^+^), (^-^), (*^), negated)
import Test.Linear.Arbitrary (ArbV2(..), ArbV3(..), ArbV4(..), approxEq, approxEqLoose)
import Test.QuickCheck (Result, quickCheck, (<?>))

-- | All Additive tests
runAdditiveTests :: Effect Unit
runAdditiveTests = do
  log "Testing Additive laws for V2..."
  testAdditiveV2
  log "Testing Additive laws for V3..."
  testAdditiveV3
  log "Testing Additive laws for V4..."
  testAdditiveV4
  log "Testing scalar operations..."
  testScalarOps

-- V2 Tests
testAdditiveV2 :: Effect Unit
testAdditiveV2 = do
  log "  left identity: zero ^+^ v = v"
  quickCheck propLeftIdentityV2
  log "  right identity: v ^+^ zero = v"
  quickCheck propRightIdentityV2
  log "  associativity: (u ^+^ v) ^+^ w = u ^+^ (v ^+^ w)"
  quickCheck propAssociativityV2
  log "  commutativity: u ^+^ v = v ^+^ u"
  quickCheck propCommutativityV2
  log "  subtraction: v ^-^ v = zero"
  quickCheck propSubtractionV2
  log "  negation: v ^+^ negated v = zero"
  quickCheck propNegationV2

propLeftIdentityV2 :: ArbV2 -> Result
propLeftIdentityV2 (ArbV2 v) =
  let result = zero ^+^ v
  in v2Eq result v <?> "zero ^+^ v should equal v"

propRightIdentityV2 :: ArbV2 -> Result
propRightIdentityV2 (ArbV2 v) =
  let result = v ^+^ zero
  in v2Eq result v <?> "v ^+^ zero should equal v"

propAssociativityV2 :: ArbV2 -> ArbV2 -> ArbV2 -> Result
propAssociativityV2 (ArbV2 u) (ArbV2 v) (ArbV2 w) =
  let left = (u ^+^ v) ^+^ w
      right = u ^+^ (v ^+^ w)
  in v2Eq left right <?> "addition should be associative"

propCommutativityV2 :: ArbV2 -> ArbV2 -> Result
propCommutativityV2 (ArbV2 u) (ArbV2 v) =
  let left = u ^+^ v
      right = v ^+^ u
  in v2Eq left right <?> "addition should be commutative"

propSubtractionV2 :: ArbV2 -> Result
propSubtractionV2 (ArbV2 v) =
  let result = v ^-^ v
  in v2Eq result zero <?> "v - v should equal zero"

propNegationV2 :: ArbV2 -> Result
propNegationV2 (ArbV2 v) =
  let result = v ^+^ negated v
  in v2Eq result zero <?> "v + (-v) should equal zero"

-- V3 Tests
testAdditiveV3 :: Effect Unit
testAdditiveV3 = do
  log "  left identity"
  quickCheck propLeftIdentityV3
  log "  right identity"
  quickCheck propRightIdentityV3
  log "  associativity"
  quickCheck propAssociativityV3
  log "  commutativity"
  quickCheck propCommutativityV3
  log "  subtraction"
  quickCheck propSubtractionV3

propLeftIdentityV3 :: ArbV3 -> Result
propLeftIdentityV3 (ArbV3 v) = v3Eq (zero ^+^ v) v <?> "left identity"

propRightIdentityV3 :: ArbV3 -> Result
propRightIdentityV3 (ArbV3 v) = v3Eq (v ^+^ zero) v <?> "right identity"

propAssociativityV3 :: ArbV3 -> ArbV3 -> ArbV3 -> Result
propAssociativityV3 (ArbV3 u) (ArbV3 v) (ArbV3 w) = v3Eq ((u ^+^ v) ^+^ w) (u ^+^ (v ^+^ w)) <?> "associativity"

propCommutativityV3 :: ArbV3 -> ArbV3 -> Result
propCommutativityV3 (ArbV3 u) (ArbV3 v) = v3Eq (u ^+^ v) (v ^+^ u) <?> "commutativity"

propSubtractionV3 :: ArbV3 -> Result
propSubtractionV3 (ArbV3 v) = v3Eq (v ^-^ v) zero <?> "subtraction"

-- V4 Tests
testAdditiveV4 :: Effect Unit
testAdditiveV4 = do
  log "  left identity"
  quickCheck propLeftIdentityV4
  log "  right identity"
  quickCheck propRightIdentityV4
  log "  associativity"
  quickCheck propAssociativityV4
  log "  commutativity"
  quickCheck propCommutativityV4

propLeftIdentityV4 :: ArbV4 -> Result
propLeftIdentityV4 (ArbV4 v) = v4Eq (zero ^+^ v) v <?> "left identity"

propRightIdentityV4 :: ArbV4 -> Result
propRightIdentityV4 (ArbV4 v) = v4Eq (v ^+^ zero) v <?> "right identity"

propAssociativityV4 :: ArbV4 -> ArbV4 -> ArbV4 -> Result
propAssociativityV4 (ArbV4 u) (ArbV4 v) (ArbV4 w) = v4Eq ((u ^+^ v) ^+^ w) (u ^+^ (v ^+^ w)) <?> "associativity"

propCommutativityV4 :: ArbV4 -> ArbV4 -> Result
propCommutativityV4 (ArbV4 u) (ArbV4 v) = v4Eq (u ^+^ v) (v ^+^ u) <?> "commutativity"

-- Scalar operations
testScalarOps :: Effect Unit
testScalarOps = do
  log "  scalar multiplication distributes: s *^ (u ^+^ v) = s *^ u ^+^ s *^ v"
  quickCheck propScalarDistributes
  log "  scalar multiplication by 1: 1.0 *^ v = v"
  quickCheck propScalarIdentity
  log "  scalar multiplication by 0: 0.0 *^ v = zero"
  quickCheck propScalarZero
  log "  lerp endpoints: lerp 0 a b = a, lerp 1 a b = b"
  quickCheck propLerpEndpoints

propScalarDistributes :: Number -> ArbV3 -> ArbV3 -> Result
propScalarDistributes s (ArbV3 u) (ArbV3 v) =
  let left = s *^ (u ^+^ v)
      right = (s *^ u) ^+^ (s *^ v)
  in v3EqLoose left right <?> "scalar multiplication should distribute"

propScalarIdentity :: ArbV3 -> Result
propScalarIdentity (ArbV3 v) = v3Eq (1.0 *^ v) v <?> "1 *^ v = v"

propScalarZero :: ArbV3 -> Result
propScalarZero (ArbV3 v) = v3Eq (0.0 *^ v) zero <?> "0 *^ v = zero"

propLerpEndpoints :: ArbV3 -> ArbV3 -> Result
propLerpEndpoints (ArbV3 a) (ArbV3 b) =
  let atZero = lerp 0.0 a b
      atOne = lerp 1.0 a b
  in v3Eq atZero a && v3Eq atOne b <?> "lerp endpoints"

-- Helper functions for approximate equality
v2Eq :: V2 Number -> V2 Number -> Boolean
v2Eq (V2 x1 y1) (V2 x2 y2) = approxEq x1 x2 && approxEq y1 y2

v3Eq :: V3 Number -> V3 Number -> Boolean
v3Eq (V3 x1 y1 z1) (V3 x2 y2 z2) = approxEq x1 x2 && approxEq y1 y2 && approxEq z1 z2

v3EqLoose :: V3 Number -> V3 Number -> Boolean
v3EqLoose (V3 x1 y1 z1) (V3 x2 y2 z2) = approxEqLoose x1 x2 && approxEqLoose y1 y2 && approxEqLoose z1 z2

v4Eq :: V4 Number -> V4 Number -> Boolean
v4Eq (V4 x1 y1 z1 w1) (V4 x2 y2 z2 w2) =
  approxEq x1 x2 && approxEq y1 y2 && approxEq z1 z2 && approxEq w1 w2
