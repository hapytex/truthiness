module Data.Truth.Prelude (
    ifThenElse, not, (&&), (||)
  ) where

import Prelude hiding (not, (&&), (||))
import Data.Truth.Class(Truthy(falsy), ifTruth, truthyAnd, truthyOr)

ifThenElse :: Truthy 𝕓 => 𝕓 -> a -> a -> a
ifThenElse = ifTruth

(&&) :: Truthy 𝕓 => 𝕓 -> 𝕓 -> 𝕓
(&&) = truthyAnd

not :: Truthy 𝕓 => 𝕓 -> Bool
not = falsy

(||) :: Truthy 𝕓 => 𝕓 -> 𝕓 -> 𝕓
(||) = truthyOr
