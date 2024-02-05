module Ch9 where

-- main = do
--   contents <- getContents
--   putStrLn $ shortLinesOnly contents

main = interact shortLinesOnly

shortLinesOnly :: String -> String
shortLinesOnly = unlines . filter (\line -> length line < 15) . lines

respondPalindrome :: String -> String
respondPalindrome =
  unlines
    . map (\xs -> if isPal xs then "Palindrome" else "Not palindrome")
    . lines

isPal xs = xs == reverse xs
