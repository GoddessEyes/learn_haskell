module Ch12.Ch12 where

import qualified Data.Foldable as F
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

-- Maybe monoids:
-- Nothing `mappend` Just "qwe"
-- Just LT `mappend` Nothing
-- getFirst $ First (Just "ASD") `mappend` First (Just "DSA")
-- getFirst . mconcat . map First $ [Nothing, Just 9, Just 10]
-- getLast . mconcat . map Last $ [Nothing, Just 9, Just 10]

-- Свёртка Maybe:
-- F.foldl (+) 2 (Just 9)

data Tree a = EmptyTree | Node a (Tree a) (Tree a) deriving (Show)

instance F.Foldable Tree where
  foldMap _ EmptyTree = mempty
  foldMap f (Node x l r) = F.foldMap f l `mappend` f x `mappend` F.foldMap f r

testTree :: Tree Integer
testTree =
  Node
    5
    ( Node
        3
        (Node 1 EmptyTree EmptyTree)
        (Node 6 EmptyTree EmptyTree)
    )
    ( Node
        9
        (Node 8 EmptyTree EmptyTree)
        (Node 10 EmptyTree EmptyTree)
    )

-- F.foldl (+) 0 testTree
-- F.foldl (*) 1 testTree
-- getAny $ F.foldMap (\x -> Any $ x == 3) testTree
-- F.foldMap (\x -> [x]) testTree
