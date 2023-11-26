module Ch3
  ( lucky,
    factorial,
    addVectors,
    first,
    second,
    third,
    pairs,
    head',
    tell,
    bmiTell,
    max',
    myCompare,
    calcBmis,
    cylinder,
  )
where

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

--bmiTell :: Double -> Double -> String
--bmiTell weight height
--  | weight / height ^ 2 <= 18.5 = "Тощий"
--  | weight / height ^ 2 <= 25.0 = "Норма"
--  | weight / height ^ 2 <= 30.0 = "Полнота"
--  | otherwise = "Лишний вес"

max' :: (Ord a) => a -> a -> a
max' a b
  | a >= b = a
  | otherwise = b

myCompare :: (Ord a) => a -> a -> Ordering
myCompare a b
  | a == b = EQ
  | a >= b = GT
  | otherwise = LT

bmiTell :: Double -> Double -> String
bmiTell weight height
  | bmi <= skinny = "Тощий"
  | bmi <= normal = "Норма"
  | bmi <= fat = "Полнота"
  | otherwise = "Лишний вес"
  where
    bmi = weight / height * height
    (skinny, normal, fat) = (18.5, 25.0, 30.0)

--calcBmis :: [(Double, Double)] -> [Double]
--calcBmis bmis = [bmi w h | (w, h) <- bmis]
--  where
--    bmi weight height = weight / height * height

cylinder :: Double -> Double -> Double
cylinder r h =
  let sideArea = 2 * pi * r * h
      topArea = pi * r * r
   in sideArea + 2 * topArea

calcBmis :: [(Double, Double)] -> [Double]
calcBmis bmis = [bmi | (w, h) <- bmis, let bmi = w / h * h, bmi > 25.0]
