module Ch7
  ( Point (..),
    Shape (..),
    area,
    nudge,
    baseCircle,
    baseRect,
  )
where

import Data.Map qualified as Map

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

data Person = Person
  { firstName :: String,
    lastName :: String,
    age :: Int,
    height :: Float,
    phoneNumber :: String,
    flavor :: String
  }
  deriving (Show, Eq, Read)

data Car = Car
  { company :: String,
    model :: String,
    year :: Int
  }
  deriving (Show)

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
  deriving
    ( Eq,
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

-- type AssocList k v = [(k, v)]
-- type IntMap v = Map Int v

data LockerState = Taken | Free deriving (Show, Eq)

type Code = String

type LockerMap = Map.Map Int (LockerState, Code)

lockerLookup :: Int -> LockerMap -> Either String Code
lockerLookup lockerNumber map =
  case Map.lookup lockerNumber map of
    Nothing -> Left $ "Locker # " ++ show lockerNumber ++ " not exist"
    Just (state, code) ->
      if state /= Taken then Right code else Left $ "Locker # " ++ show lockerNumber ++ " taken"

lockers :: LockerMap
lockers =
  Map.fromList
    [ (100, (Taken, "ZD39I")),
      (101, (Free, "QWE3R"))
    ]

-- data List a = Empty | Cons {listHead :: a, listTail :: List a} deriving (Show, Read, Eq, Ord)

infixr 5 :-:

-- 3 :-: (4 :-: (5 :-: Empty))
data List a = Empty | a :-: (List a) deriving (Show, Read, Eq, Ord)

infixr 5 ^++

(^++) :: List a -> List a -> List a
Empty ^++ ys = ys
(x :-: xs) ^++ ys = x :-: (xs ^++ ys)

--
-- ghci> let a = 3 :-: 4 :-: Empty
-- ghci> let b = 6 :-: 5 :-: Empty
-- ghci> a ^++ b
