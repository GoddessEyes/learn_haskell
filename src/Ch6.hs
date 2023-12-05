module Ch6
  ( numUniques,
    wordsNum,
    isIn,
  )
where

import Data.List (nub, group, sort, isPrefixOf, any, tails)

numUniques :: (Eq a) => [a] -> Int
numUniques = length . nub

wordsNum :: String -> [(String, Int)]
wordsNum = map(\ws -> (head ws, length ws)) . group . sort . words

isIn :: (Eq a) => [a] -> [a] -> Bool
needle `isIn` haystack = any (needle `isPrefixOf`) (tails haystack)
