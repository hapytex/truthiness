{-# LANGUAGE FlexibleInstances, Safe, UndecidableInstances #-}

module Data.Truth.Class where

import Control.Applicative(Alternative)
import Control.Monad(guard)

class Truthy a where
  truthy :: a -> Bool
  truthy = not . falsy

  falsy :: a -> Bool
  falsy = not . truthy

  {-# MINIMAL truthy | falsy #-}

ifTruth :: Truthy bool => bool -> a -> a -> a
ifTruth c
  | truthy c = const
  | otherwise = const id

ifThenElse :: Truthy bool => bool -> a -> a -> a
ifThenElse = ifTruth  -- used for rebindable syntax

truthyAnd :: Truthy bool => bool -> bool -> bool
truthyAnd x
  | truthy x = const x
  | otherwise = id

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
