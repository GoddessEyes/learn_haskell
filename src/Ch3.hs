module Ch3 (lucky, factorial, addVectors, first, second, third, pairs, head', tell) where

lucky :: Integer -> String
lucky 7 = "Luck"
lucky _ = "Badluck"

factorial :: Integer -> Integer
factorial 0 = 1
factorial n = n * factorial (n - 1)

addVectors :: (Double, Double) -> (Double, Double) -> (Double, Double)
addVectors (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

first :: (a, b, c) -> a
first (x, _, _) = x

second :: (a, b, c) -> b
second (_, y, _) = y

third :: (a, b, c) -> c
third (_, _, z) = z

xs :: [(Integer, Integer)]
xs = [(1, 2), (3, 4), (5, 6)]

pairs :: [Integer]
pairs = [x + y | (x, y) <- xs]

head' :: [a] -> a
head' [] = error "Empty list"
head' (x : _) = x

tell :: (Show a) => [a] -> String
tell [] = "Empty list"
tell [x] = "One elem in list " ++ show x
tell [x, y] = "Two elem in list " ++ show x ++ " and " ++ show y
tell (x : y : _) = "More then two elem in list. First two elems: " ++ show x ++ " and " ++ show y
