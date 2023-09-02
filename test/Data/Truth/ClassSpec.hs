module Data.Truth.ClassSpec where

import Test.Hspec (describe, it)
import Test.Hspec.Discover (Spec)
import Test.QuickCheck (property)
import Data.Bits(xor)
import Data.Truth.Class(truthyAnd, truthyOr, truthyXor)

sameFunc :: (Bool -> Bool -> Bool) -> (Bool -> Bool -> Bool) -> Bool -> Bool -> Bool
sameFunc f g = go
  where go x y = f x y == g x y

spec :: Spec
spec = do
  describe "equalty with Bool" $ do
    it "(&&)" (property (sameFunc (&&) truthyAnd))
    it "(||)" (property (sameFunc (||) truthyOr))
    it "(xor)" (property (sameFunc xor truthyXor))
