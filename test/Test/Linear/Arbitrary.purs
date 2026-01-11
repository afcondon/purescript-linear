-- | Arbitrary instances for property testing.
module Test.Linear.Arbitrary where

import Prelude

import Data.Number (abs, sqrt)
import Linear.V2 (V2(..))
import Linear.V3 (V3(..))
import Linear.V4 (V4(..))
import Linear.Quaternion (Quaternion(..))
import Test.QuickCheck (class Arbitrary, arbitrary)
import Test.QuickCheck.Gen (Gen, choose)

-- | Generate a reasonable floating point number (avoiding extremes)
genNumber :: Gen Number
genNumber = choose (-1000.0) 1000.0

-- | Generate a small number for testing near-zero behavior
genSmallNumber :: Gen Number
genSmallNumber = choose (-1.0) 1.0

-- | Wrapper for V2 with Arbitrary instance
newtype ArbV2 = ArbV2 (V2 Number)

instance Arbitrary ArbV2 where
  arbitrary = ArbV2 <$> (V2 <$> genNumber <*> genNumber)

unwrapV2 :: ArbV2 -> V2 Number
unwrapV2 (ArbV2 v) = v

-- | Wrapper for V3 with Arbitrary instance
newtype ArbV3 = ArbV3 (V3 Number)

instance Arbitrary ArbV3 where
  arbitrary = ArbV3 <$> (V3 <$> genNumber <*> genNumber <*> genNumber)

unwrapV3 :: ArbV3 -> V3 Number
unwrapV3 (ArbV3 v) = v

-- | Wrapper for V4 with Arbitrary instance
newtype ArbV4 = ArbV4 (V4 Number)

instance Arbitrary ArbV4 where
  arbitrary = ArbV4 <$> (V4 <$> genNumber <*> genNumber <*> genNumber <*> genNumber)

unwrapV4 :: ArbV4 -> V4 Number
unwrapV4 (ArbV4 v) = v

-- | A unit quaternion (normalized) for rotation testing
newtype UnitQuaternion = UnitQuaternion (Quaternion Number)

instance Arbitrary UnitQuaternion where
  arbitrary = do
    w <- genNumber
    x <- genNumber
    y <- genNumber
    z <- genNumber
    let n = w * w + x * x + y * y + z * z
        sqrtN = if n < 0.0001 then 1.0 else sqrt n
    pure $ UnitQuaternion $ Quaternion (w / sqrtN) (V3 (x / sqrtN) (y / sqrtN) (z / sqrtN))

-- | A non-zero vector for testing normalization
newtype NonZeroV3 = NonZeroV3 (V3 Number)

instance Arbitrary NonZeroV3 where
  arbitrary = do
    x <- genNumber
    y <- genNumber
    z <- genNumber
    let mag = x * x + y * y + z * z
    if mag < 0.0001
      then pure $ NonZeroV3 (V3 1.0 0.0 0.0)  -- fallback to unit vector
      else pure $ NonZeroV3 (V3 x y z)

-- | A non-zero V2 for testing
newtype NonZeroV2 = NonZeroV2 (V2 Number)

instance Arbitrary NonZeroV2 where
  arbitrary = do
    x <- genNumber
    y <- genNumber
    let mag = x * x + y * y
    if mag < 0.0001
      then pure $ NonZeroV2 (V2 1.0 0.0)
      else pure $ NonZeroV2 (V2 x y)

-- | Helper: check if two numbers are approximately equal
approxEq :: Number -> Number -> Boolean
approxEq a b = abs (a - b) < 1.0e-6

-- | Helper: check if two numbers are approximately equal with larger tolerance
approxEqLoose :: Number -> Number -> Boolean
approxEqLoose a b = abs (a - b) < 1.0e-3
