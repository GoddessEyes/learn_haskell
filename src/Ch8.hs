module Ch8 (main) where

main = putStrLn "Hello world"

reverseWords' = do
  line <- getLine
  if null line
    then return ()
    else do
      putStrLn $ reverseWords line
      reverseWords'

reverseWords :: String -> String
reverseWords = unwords . map reverse . words
