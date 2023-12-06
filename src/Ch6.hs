module Ch6
  ( numUniques,
    wordsNum,
    isIn,
    encodeCesar,
  )
where

import Data.List (nub, group, sort, isPrefixOf, tails)
import Data.Char (ord, chr)

numUniques :: (Eq a) => [a] -> Int
numUniques = length . nub

wordsNum :: String -> [(String, Int)]
wordsNum = map(\ws -> (head ws, length ws)) . group . sort . words

isIn :: (Eq a) => [a] -> [a] -> Bool
needle `isIn` haystack = any (needle `isPrefixOf`) (tails haystack)

encodeCesar :: Int -> String -> String
encodeCesar offset msg = map (\c -> chr $ ord c + offset) msg

decodeCesar :: Int -> String -> String
decodeCesar shift msg = encodeCesar (negate shift) msg
