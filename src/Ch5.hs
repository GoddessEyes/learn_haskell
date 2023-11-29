module Ch5 (multThree, compareWithHundred, dividedByTen, applyTwice, zipWith') where

multThree :: Int -> Int -> Int -> Int
multThree x y z = x * y * z

-- new func with Int -> Int -> Int signature
--multTwoWithNine = multThree 9

compareWithHundred :: Int -> Ordering
--compareWithHundred x = x `compare` 100
compareWithHundred = compare 100

dividedByTen :: (Floating a) => a -> a
dividedByTen = (/10)

applyTwice :: (a -> a) -> a -> a
applyTwice f x = f (f x)

--(+) [1,2,3] [4,5,6]
zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' _ [] _ = []
zipWith' _ _ [] = []
zipWith' f (x:xs) (y:ys) = f x y : zipWith' f xs ys
