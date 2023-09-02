{-# LANGUAGE DefaultSignatures, FlexibleInstances, Safe, UndecidableInstances #-}

module Data.Truth.Class where

import Control.Applicative(Alternative)
import Control.Monad(guard)

class Truthy 𝕓 where
  truthy :: 𝕓 -> Bool
  truthy = not . falsy

  falsy :: 𝕓 -> Bool
  falsy = not . truthy

  {-# MINIMAL truthy | falsy #-}

class Truthy 𝕓 => Falsy 𝕓 where
  false :: 𝕓
  default false :: Monoid 𝕓 => 𝕓
  false = mempty
  {-# MINIMAL false #-}

notOrDefault :: Falsy a => a -> 𝕓 -> 𝕓
notOrDefault d = go
  where go x
          | Truth x = false
          | otherwise = d

ifTruth :: Truthy 𝕓 => 𝕓 -> a -> a -> a
ifTruth c
  | truthy c = const
  | otherwise = const id

ifThenElse :: Truthy 𝕓 => 𝕓 -> a -> a -> a
ifThenElse = ifTruth  -- used for rebindable syntax

truthyAnd :: Truthy bool => 𝕓 -> 𝕓 -> 𝕓
truthyAnd x
  | truthy x = const x
  | otherwise = id

truthyNor :: Falsy bool => bool -> bool -> bool
truthyNor = undefined

truthyXor :: Falsy bool => bool -> bool -> bool
truthyXor x y
  | tx && ty = false
  | tx = x
  | otherwise = ty
  where tx = truthy x
        ty = truthy y

turthyNand :: Falsy bool -> bool -> bool
truthyNand = undefined

truthyAnd' :: Truthy bool₁ => bool₁ -> bool₂ -> Either bool₁ bool₂
truthyAnd' x
  | truthy x = const (Left x)
  | otherwise = Right

truthyOr :: Truthy bool => bool -> bool -> bool
truthyOr x
  | truthy x = id
  | otherwise = const x

truthyOr' :: Truthy bool₁ => bool₁ -> bool₂ -> Either bool₁ bool₂
truthyOr' x
  | truthy x = Right
  | otherwise = const (Left x)

guardTruth :: (Alternative f, Truthy bool) => bool -> f ()
guardTruth = guard . truthy

(∧) :: Truthy bool => bool -> bool -> bool
(∧) = truthyAnd

(∨) :: Truthy bool => bool -> bool -> bool
(∨) = truthyOr

(⊻) :: Truthy bool => bool -> bool -> bool
(⊻) = truthyXor

(⊼) :: Falsy bool => bool -> bool -> bool
(⊼) = truthyNand

(⊽) :: Falsy bool => bool -> bool -> bool
(⊽) = truthyNor

instance {-# OVERLAPPABLE #-} Foldable f => Truthy (f a) where
  falsy = null

instance {-# OVERLAPPABLE #-} (Eq a, Num a) => Truthy a where
  truthy 0 = False
  truthy _ = True
  falsy 0 = True
  falsy _ = False

instance Truthy Bool where
  truthy = id
  falsy = not

instance Truthy a => Truthy (Maybe a) where
  truthy = maybe False truthy
  falsy = maybe True falsy

instance (Truthy x, Truthy y) => Truthy (Either x y) where
  truthy = either truthy truthy
  falsy = either falsy falsy

instance Truthy () where
  truthy _ = False
  falsy _ = True

instance Truthy [a] where
  falsy = null
