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
    lNumLongChains,
    lFlip,
    sum',
    elem',
    map',
    maximum',
    twExs,
    filter',
    product',
    reverse',
    last'
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


lNumLongChains :: Int
lNumLongChains = length (filter (\xs -> length xs > 15) (map chain [1..100]))

lFlip :: (a -> b -> c) -> b -> a -> c
lFlip f = \x y -> f y x

sum' :: (Num a) => [a] -> a
sum' = foldl ((+)) 0

elem' :: (Eq a) => a -> [a] -> Bool
elem' y ys = foldl (\acc x -> if x == y then True else acc) False ys

-- map' (\x -> x + 1) [1,2,3] || map' (+1) [1,2,3]
map' :: (a -> b) -> [a] -> [b]
map' f xs = foldr (\x acc -> f x : acc) [] xs

maximum' :: (Ord a) => [a] -> a
--maximum' xs = foldl1 (\x acc -> if x > acc then x else acc) xs

maximum' = foldl1 max


reverse' :: [a] -> [a]
--reverse' = foldl (\acc x -> x : acc) []
reverse' = foldl (flip (:)) []

product' :: (Num a) => [a] -> a
product' = foldl (*) 1

filter' :: (a -> Bool) -> [a] -> [a]
filter' p = foldr (\x acc -> if p x then x : acc else acc) []

last' :: [a] -> a
last' = foldr1 (\acc x -> x)

and' :: [Bool] -> Bool
and' xs = foldr (&&) True xs

sqrtSums :: Int
sqrtSums = length (takeWhile (< 1000) (scanl1 (+) (map sqrt [1..]))) + 1

oddSquareSum :: Integer
oddSquareSum = sum . takeWhile (<10000) . filter odd $ map (^ 2) [1..]
