module Ch6
  ( numUniques,
    wordsNum,
    isIn,
    encodeCesar,
  )
where

import Data.Char (chr, digitToInt, ord)
import Data.List (find, group, isPrefixOf, nub, sort, tails)

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
