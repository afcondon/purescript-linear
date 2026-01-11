-- | The Additive typeclass for vector spaces.
-- |
-- | This module provides the foundational `Additive` typeclass that represents
-- | vectors as an additive group, along with scalar multiplication operations.
module Linear.Vector
  ( class Additive
  , zero
  , add
  , sub
  , lerp
  , liftU2
  , liftI2
  , (^+^)
  , (^-^)
  , scalarR
  , (^*)
  , scalarL
  , (*^)
  , scalarDiv
  , (^/)
  , negated
  , sumV
  ) where

import Prelude

import Data.Foldable (class Foldable, foldl)

-- | A vector space with addition and scalar multiplication.
-- |
-- | Laws:
-- | - `add zero v = v` (left identity)
-- | - `add v zero = v` (right identity)
-- | - `add (add u v) w = add u (add v w)` (associativity)
-- | - `add u v = add v u` (commutativity)
-- | - `sub v v = zero`
class Functor f <= Additive f where
  -- | The zero vector
  zero :: forall a. Semiring a => f a
  -- | Vector addition
  add :: forall a. Semiring a => f a -> f a -> f a
  -- | Vector subtraction
  sub :: forall a. Ring a => f a -> f a -> f a
  -- | Linear interpolation: `lerp t a b = a + t * (b - a)`
  -- |
  -- | - `lerp 0.0 a b = a`
  -- | - `lerp 1.0 a b = b`
  lerp :: forall a. Ring a => a -> f a -> f a -> f a
  -- | Apply a function to non-zero values (union semantics)
  liftU2 :: forall a. (a -> a -> a) -> f a -> f a -> f a
  -- | Apply a function component-wise (intersection semantics)
  liftI2 :: forall a b c. (a -> b -> c) -> f a -> f b -> f c

infixl 6 add as ^+^
infixl 6 sub as ^-^

-- | Right scalar multiplication
-- |
-- | ```purescript
-- | V2 1.0 2.0 ^* 3.0 = V2 3.0 6.0
-- | ```
scalarR :: forall f a. Functor f => Semiring a => f a -> a -> f a
scalarR v s = map (_ * s) v

infixl 7 scalarR as ^*

-- | Left scalar multiplication
-- |
-- | ```purescript
-- | 3.0 *^ V2 1.0 2.0 = V2 3.0 6.0
-- | ```
scalarL :: forall f a. Functor f => Semiring a => a -> f a -> f a
scalarL s v = map (s * _) v

infixl 7 scalarL as *^

-- | Right scalar division
-- |
-- | ```purescript
-- | V2 6.0 9.0 ^/ 3.0 = V2 2.0 3.0
-- | ```
scalarDiv :: forall f a. Functor f => EuclideanRing a => f a -> a -> f a
scalarDiv v s = map (_ / s) v

infixl 7 scalarDiv as ^/

-- | Negate a vector
-- |
-- | ```purescript
-- | negated (V2 1.0 (-2.0)) = V2 (-1.0) 2.0
-- | ```
negated :: forall f a. Functor f => Ring a => f a -> f a
negated = map negate

-- | Sum a collection of vectors
sumV :: forall f g a. Foldable f => Additive g => Semiring a => f (g a) -> g a
sumV = foldl add zero
