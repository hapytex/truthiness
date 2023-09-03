module Data.Truth.Prelude (
    ifThenElse, not, (&&), (||), or
  ) where

import Prelude hiding (not, (&&), (||))
import Data.Truth.Class(Truthy(falsy), ifTruth, truthyAnd, truthyOr, truthyOrF)

ifThenElse :: Truthy 𝕓 => 𝕓 -> a -> a -> a
ifThenElse = ifTruth

not :: Truthy 𝕓 => 𝕓 -> Bool
not = falsy

(&&) :: Truthy 𝕓 => 𝕓 -> 𝕓 -> 𝕓
(&&) = truthyAnd

(||) :: Truthy 𝕓 => 𝕓 -> 𝕓 -> 𝕓
(||) = truthyOr

or :: (Foldable f, Falsy 𝕓) => f 𝕓 -> 𝕓
or = truthOrF
