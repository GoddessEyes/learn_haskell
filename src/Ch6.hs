module Ch6
  ( numUniques,
    wordsNum,
    isIn,
    encodeCesar,
    firstTo,
    firstTo40,
    decodeCesar,
    findKey',
    phoneBook,
    phoneBook',
    string2digits,
  )
where

import Data.Char (chr, digitToInt, isDigit, ord)
import Data.List (find, group, isPrefixOf, nub, sort, tails)
import qualified Data.Map as Map

numUniques :: (Eq a) => [a] -> Int
numUniques = length . nub

wordsNum :: String -> [(String, Int)]
wordsNum = map (\ws -> (head ws, length ws)) . group . sort . words

isIn :: (Eq a) => [a] -> [a] -> Bool
needle `isIn` haystack = any (needle `isPrefixOf`) (tails haystack)

encodeCesar :: Int -> String -> String
encodeCesar offset msg = map (\c -> chr $ ord c + offset) msg

decodeCesar :: Int -> String -> String
decodeCesar shift msg = encodeCesar (negate shift) msg

digitSum :: Int -> Int
digitSum = sum . map digitToInt . show

firstTo40 :: Maybe Int
firstTo40 = find (\x -> digitSum x == 40) [1 ..]

firstTo :: Int -> Maybe Int
firstTo n = find (\x -> digitSum x == n) [1 ..]

phoneBook' :: [(String, String)]
phoneBook' =
  [ ("Olya", "555"),
    ("Zhenya", "666"),
    ("katya", "555")
  ]

findKey' :: (Eq k) => k -> [(k, v)] -> Maybe v

-- findKey key [] = Nothing
-- findKey' key ((k, v) : xs)

-- | key == k = Just v
-- | otherwise = findKey' key xs
findKey' key = foldr (\(k, v) acc -> if key == k then Just v else acc) Nothing

phoneBook :: Map.Map String String
phoneBook =
  Map.fromList $
    [ ("test", "123-123"),
      ("test1", "234-234")
    ]

string2digits :: String -> [Int]
string2digits = map digitToInt . filter isDigit
