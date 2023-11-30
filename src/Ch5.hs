module Ch5
  ( multThree,
    compareWithHundred,
    dividedByTen,
    applyTwice,
    zipWith',
    flip',
    quickSortWithFilter,
    largestDivisible,
    chain,
    numLongChains,
  )
where

multThree :: Int -> Int -> Int -> Int
multThree x y z = x * y * z

-- new func with Int -> Int -> Int signature
--multTwoWithNine = multThree 9

compareWithHundred :: Int -> Ordering
--compareWithHundred x = x `compare` 100
compareWithHundred = compare 100

dividedByTen :: (Floating a) => a -> a
dividedByTen = (/ 10)

applyTwice :: (a -> a) -> a -> a
applyTwice f x = f (f x)

--(+) [1,2,3] [4,5,6]
zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' _ [] _ = []
zipWith' _ _ [] = []
zipWith' f (x : xs) (y : ys) = f x y : zipWith' f xs ys

flip' :: (a -> b -> c) -> (b -> a -> c)
--flip' f = g
--  where g x y = f y x

-- flip' zip [1, 2, 3, 4, 5, 6] "Hello!"
flip' f y x = f x y

-- Bult-in func
--map (+3) [1, 2, 3]
--filter (>3) [1, 2, 3, 4]

quickSortWithFilter :: (Ord a) => [a] -> [a]
quickSortWithFilter [] = []
quickSortWithFilter (x : xs) =
  let smallerSorted = quickSortWithFilter (filter (<= x) xs)
      biggerSorted = quickSortWithFilter (filter (> x) xs)
   in smallerSorted ++ [x] ++ biggerSorted

largestDivisible :: Integer
largestDivisible = head (filter p [100000, 99999 ..])
  where
    p x = x `mod` 3829 == 0

twExs :: Integer
twExs = sum (takeWhile (< 10000) (filter odd (map (^ 2) [1 ..])))

chain :: Int -> [Int]
chain 1 = [1]
chain n
  | even n = n : chain (n `div` 2)
  | odd n = n : chain (n * 3 + 1)

numLongChains :: Int
numLongChains = length (filter isLong (map chain [1..100]))
  where isLong xs = length xs > 15
