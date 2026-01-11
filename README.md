# purescript-linear

A PureScript port of Haskell's [linear](https://hackage.haskell.org/package/linear) library, providing fixed-size vectors, matrices, and quaternions for graphics and game programming.

## Features

- **Fixed-size vectors**: `V2`, `V3`, `V4` with full typeclass instances
- **Typeclass hierarchy**: `Additive` and `Metric` for generic vector operations
- **Quaternions**: 3D rotations with `axisAngle`, `rotate`, and `slerp` interpolation
- **Matrices**: `M22`, `M33`, `M44` with determinant, inverse, and transformations
- **Affine geometry**: `Point` type with proper point/vector distinction
- **Pure PureScript**: No FFI dependencies

## Installation

```bash
spago install linear
```

Or add to your `spago.yaml`:

```yaml
dependencies:
  - linear
```

## Quick Start

```purescript
import Linear.V3 (V3(..), cross)
import Linear.Metric (dot, norm, signorm)
import Linear.Vector ((^+^), (^-^), (*^))
import Linear.Quaternion (axisAngle, rotate)
import Data.Number (pi)

-- Vector arithmetic
let a = V3 1.0 2.0 3.0
    b = V3 4.0 5.0 6.0
    sum = a ^+^ b           -- V3 5.0 7.0 9.0
    scaled = 2.0 *^ a       -- V3 2.0 4.0 6.0

-- Metric operations
let length = norm a         -- sqrt(14)
    unit = signorm a        -- normalized to length 1
    d = dot a b             -- 32.0

-- Cross product (V3 only)
let perpendicular = cross a b

-- Quaternion rotation (90 degrees around Y axis)
let q = axisAngle (V3 0.0 1.0 0.0) (pi / 2.0)
    v = V3 1.0 0.0 0.0
    rotated = rotate q v    -- approximately V3 0.0 0.0 (-1.0)
```

## Modules

### Core Types

| Module | Description |
|--------|-------------|
| `Linear.V2` | 2D vectors with `perp`, `angle`, `crossZ` |
| `Linear.V3` | 3D vectors with `cross` product |
| `Linear.V4` | 4D vectors for homogeneous coordinates |
| `Linear.Quaternion` | Quaternion rotations and interpolation |
| `Linear.Matrix` | M22, M33, M44 matrix types and operations |

### Typeclasses

| Module | Description |
|--------|-------------|
| `Linear.Vector` | `Additive` typeclass for vector spaces |
| `Linear.Metric` | `Metric` typeclass for inner product spaces |
| `Linear.Affine` | `Affine` typeclass for point/vector distinction |
| `Linear.Epsilon` | `Epsilon` typeclass for near-zero testing |

### Re-exports

| Module | Description |
|--------|-------------|
| `Linear` | Re-exports all modules for convenience |

## API Overview

### Additive (Linear.Vector)

```purescript
class Functor f <= Additive f where
  zero :: forall a. Semiring a => f a
  (^+^) :: forall a. Semiring a => f a -> f a -> f a
  (^-^) :: forall a. Ring a => f a -> f a -> f a
  lerp :: forall a. Ring a => a -> f a -> f a -> f a

-- Scalar operations
(*^) :: forall f a. Functor f => Semiring a => a -> f a -> f a
(^*) :: forall f a. Functor f => Semiring a => f a -> a -> f a
(^/) :: forall f a. Functor f => EuclideanRing a => f a -> a -> f a
negated :: forall f a. Functor f => Ring a => f a -> f a
```

### Metric (Linear.Metric)

```purescript
class Additive f <= Metric f where
  dot :: forall a. Semiring a => f a -> f a -> a
  quadrance :: forall a. Semiring a => f a -> a      -- |v|^2
  qd :: forall a. Ring a => f a -> f a -> a          -- |u-v|^2
  distance :: f Number -> f Number -> Number
  norm :: f Number -> Number
  signorm :: f Number -> f Number                    -- unit vector
```

### Quaternion (Linear.Quaternion)

```purescript
data Quaternion a = Quaternion a (V3 a)

axisAngle :: V3 Number -> Number -> Quaternion Number
rotate :: Quaternion Number -> V3 Number -> V3 Number
slerp :: Number -> Quaternion Number -> Quaternion Number -> Quaternion Number
conjugate :: forall a. Ring a => Quaternion a -> Quaternion a
qmul :: forall a. Ring a => Quaternion a -> Quaternion a -> Quaternion a
```

### Matrix (Linear.Matrix)

```purescript
type M22 a = V2 (V2 a)
type M33 a = V3 (V3 a)
type M44 a = V4 (V4 a)

(!*!) :: M33 Number -> M33 Number -> M33 Number     -- matrix multiply
(!*) :: forall f a. ... => f (f a) -> f a -> f a    -- matrix-vector
transpose33 :: forall a. M33 a -> M33 a
identity33 :: forall a. Semiring a => M33 a
det33 :: forall a. Ring a => M33 a -> a
inv33 :: M33 Number -> Maybe (M33 Number)
fromQuaternion :: Quaternion Number -> M33 Number
mkTransformation :: Quaternion Number -> V3 Number -> M44 Number
```

## Differences from Haskell's linear

| Haskell | PureScript | Notes |
|---------|------------|-------|
| `Num a` | `Semiring a` | `+`, `*`, `zero`, `one` |
| `Num a` (subtraction) | `Ring a` | Adds `sub`, `negate` |
| `Fractional a` | `EuclideanRing a` | Division |
| `Floating a` | `Number` (concrete) | No typeclass, uses Number directly |
| Lenses | Not included | Can be added with profunctor-lenses |

## Testing

The library includes property-based tests verifying algebraic laws:

```bash
spago test
```

Tests cover:
- Additive laws (identity, associativity, commutativity) for V2, V3, V4
- Metric laws (dot product properties, triangle inequality)
- Cross product properties (anti-commutativity, perpendicularity)
- Quaternion algebra (associativity, rotation composition)
- Slerp interpolation correctness

## License

BSD-3-Clause (same as the original Haskell library) - see [LICENSE](LICENSE)

## Contributing

Contributions welcome! Please ensure all tests pass before submitting PRs.

## Acknowledgments

This is a port of Edward Kmett's excellent [linear](https://hackage.haskell.org/package/linear) library for Haskell.
