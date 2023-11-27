module Ch5 (multThree, compareWithHundred) where

multThree :: Int -> Int -> Int -> Int
multThree x y z = x * y * z

-- new func with Int -> Int -> Int signature
--multTwoWithNine = multThree 9

compareWithHundred :: Int -> Ordering
--compareWithHundred x = x `compare` 100
compareWithHundred = compare 100