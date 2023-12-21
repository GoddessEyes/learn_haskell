module Ch7
  ( Point (..),
    Shape (..),
    area,
    nudge,
    baseCircle,
    baseRect,
  )
where

data Point = Point Float Float deriving (Show)

data Shape = Circle Point Float | Rectangle Point Point
  deriving (Show)

area :: Shape -> Float
area (Circle _ r) = pi * r ^ 2
area (Rectangle (Point x1 y1) (Point x2 y2)) = (abs $ x2 - x1) * (abs $ y2 - y1)

nudge :: Shape -> Float -> Float -> Shape
nudge (Circle (Point x y) r) a b = Circle (Point (x + a) (y + b)) r
nudge (Rectangle (Point x1 y1) (Point x2 y2)) a b =
  Rectangle (Point (x1 + a) (y1 + b)) (Point (x2 + a) (y2 + b))

baseCircle :: Float -> Shape
baseCircle r = Circle (Point 0 0) r

baseRect :: Float -> Float -> Shape
baseRect width height = Rectangle (Point 0 0) (Point width height)

data Person = Person { firstName :: String
                     , lastName :: String
                     , age :: Int
                     , height :: Float
                     , phoneNumber :: String
                     , flavor :: String } deriving (Show, Eq, Read)




data Car = Car { company :: String
               , model :: String
               , year :: Int
               } deriving (Show)


data Vector a = Vector a a a deriving (Show)
vplus :: (Num a) => Vector a -> Vector a -> Vector a
(Vector i j k) `vplus` (Vector l m n) = Vector (i + l) (j + m) (k + n)

scalarProd :: (Num a) => Vector a -> Vector a -> a
(Vector i j k) `scalarProd` (Vector l m n) = i * l + j * m + k * n


vmult :: (Num a) => Vector a -> a -> Vector a
(Vector i j k) `vmult` m = Vector (i * m) (j * m) (k * m)

-- mysteryDude = "Person { firstName =\"Michael\"" ++
--                       ", lastName =\"Diamond\"" ++ 
--                       ", age = 43}"
--
--
-- readedPerson = read mysteryDude :: Person 
--

-- ghci> minBound :: Day 
-- Mon
-- ghci> maxBound :: Day 
-- Sun
data Day = Mon | Tue | Wed | Thu | Fri | Sat | Sun 
  deriving (
    Eq, 
    Ord, 
    Show, 
    Read, 
    Bounded, 
    Enum
  )

type Name = String
type PhoneNumber = String
type PhoneBook = [(Name, PhoneNumber)]

inPhoneBook :: Name -> PhoneNumber -> PhoneBook -> Bool
inPhoneBook name phone phoneBook = (name, phone) `elem` phoneBook

