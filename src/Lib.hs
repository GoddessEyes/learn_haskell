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
