{-# LANGUAGE DefaultSignatures, FlexibleInstances, Safe, UndecidableInstances #-}

module Data.Truth.Class (
    Truthy(truthy, falsy),
    Falsy(false),
    ifTruth, guardTruth,
    truthyAnd, truthyAnd', truthyOr, truthyOr', truthyXor,
    (∧), (∨), (⊻)
  ) where

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


ifTruth :: Truthy 𝕓 => 𝕓 -> a -> a -> a
ifTruth c
  | truthy c = const
  | otherwise = const id

truthyAnd :: Truthy 𝕓 => 𝕓 -> 𝕓 -> 𝕓
truthyAnd x = ifTruth x id (const x)

truthyAnd' :: Truthy bool₁ => bool₁ -> bool₂ -> Either bool₁ bool₂
truthyAnd' x = ifTruth x Right (const (Left x))

truthyOr :: Truthy 𝕓 => 𝕓 -> 𝕓 -> 𝕓
truthyOr x = ifTruth x (const x) id

truthyOr' :: Truthy bool₁ => bool₁ -> bool₂ -> Either bool₁ bool₂
truthyOr' x = ifTruth x (const (Left x)) Right

truthyXor :: Falsy 𝕓 => 𝕓 -> 𝕓 -> 𝕓
truthyXor x y
  | tx == ty = false
  | tx = x
  | ty = y
  where tx = truthy x
        ty = truthy y

guardTruth :: (Alternative f, Truthy 𝕓) => 𝕓 -> f ()
guardTruth = guard . truthy

(∧) :: Truthy 𝕓 => 𝕓 -> 𝕓 -> 𝕓
(∧) = truthyAnd

(∨) :: Truthy 𝕓 => 𝕓 -> 𝕓 -> 𝕓
(∨) = truthyOr

(⊻) :: Falsy 𝕓 => 𝕓 -> 𝕓 -> 𝕓
(⊻) = truthyXor

instance {-# OVERLAPPABLE #-} Foldable f => Truthy (f 𝕓) where
  falsy = null

instance {-# OVERLAPPABLE #-} (Eq 𝕓, Num 𝕓) => Truthy 𝕓 where
  truthy 0 = False
  truthy _ = True
  falsy 0 = True
  falsy _ = False

instance Truthy Bool where
  truthy = id
  falsy = not

instance Falsy Bool where
  false = False

instance Truthy a => Truthy (Maybe a) where
  truthy = maybe False truthy
  falsy = maybe True falsy

instance Truthy a => Falsy (Maybe a) where
  false = Nothing

instance (Truthy x, Truthy y) => Truthy (Either x y) where
  truthy = either truthy truthy
  falsy = either falsy falsy

instance Truthy () where
  truthy _ = False
  falsy _ = True

instance Falsy () where
  false = ()

instance Truthy [a] where
  falsy = null

instance Falsy [a] where
  false = []
