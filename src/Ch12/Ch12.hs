module Ch12.Ch12 where

import Data.Monoid


newtype Pair b a = Pair {getPair :: (a, b)} deriving (Show)

-- getPair $ fmap (*100) (Pair (2, 3))
instance Functor (Pair c) where
  fmap f (Pair (x, y)) = Pair (f x, y)


--data CoolBool = CoolBool {getCoolBool :: Bool}
newtype CoolBool = CoolBool {getCoolBool :: Bool}

helloMe :: CoolBool -> String
helloMe (CoolBool _) = "Hello"

-- Monoids examples:
--[1, 2, 3] `mappend` [4, 5, 6]
--"one two" `mappend` "three"
--mconcat [[1,2], [3, 4]]
--getAny $ Any True `mappend` Any False
--getAll $ All True `mappend` All False

productEx :: Integer
productEx =
  getProduct $ Product 3 `mappend` Product 9

sumEx :: Integer
sumEx =
  getSum $ Sum 2 `mappend` Sum 9

lengthCompare :: String -> String -> Ordering
lengthCompare x y = (length x `compare` length y) `mappend` (x `compare` y)
