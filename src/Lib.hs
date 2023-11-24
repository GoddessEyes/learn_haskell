module Lib
  ( someFunc,
    test,
    doubleMe,
    doubleUs,
    doubleSmallNumber,
    boomBangs,
    length',
    removeNonUppercase,
    nestedLists,
    triples,
    rightTriangles,
    factorial,
    circumference,
    circumference',
  )
where

someFunc :: IO ()
someFunc = putStrLn "11"

test :: Integer
test = 92 `div` 10

doubleMe :: Integer -> Integer
doubleMe x = x + x

doubleUs :: Integer -> Integer -> Integer
doubleUs x y = doubleMe x + doubleMe y

doubleSmallNumber :: Integer -> Integer
doubleSmallNumber x =
  if x > 100
    then x
    else x * 2

boomBangs :: Integral a => [a] -> [String]
boomBangs xs = [if x < 10 then "Bum!" else "Bang!" | x <- xs, odd x]

length' :: [Integer] -> Integer
length' xs = sum [1 | _ <- xs]

removeNonUppercase :: String -> String
removeNonUppercase xs = [x | x <- xs, x `elem` ['A' .. 'Z']]

nestedLists :: [[Integer]] -> [[Integer]]
nestedLists xss = [[x | x <- xs, even x] | xs <- xss]

triples :: [(Integer, Integer, Integer)]
triples = [(a, b, c) | a <- [1 .. 10], b <- [1 .. 10], c <- [1 .. 10]]

rightTriangles :: [(Integer, Integer, Integer)]
rightTriangles = [(a, b, c) | a <- [1 .. 10], b <- [1 .. 10], c <- [1 .. 10], a * a + b * b == c * c, a + b + c == 24]


factorial :: Integer -> Integer
factorial n = product [1..n]

circumference :: Float -> Float
circumference r = 2 * pi * r

circumference' :: Double -> Double
circumference'  r = 2 * pi * r
