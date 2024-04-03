module Ch8
  ( main,
    reverseWords,
    reverseWords',
    testWhenMonad,
    testSequence,
    testForever,
    colorAssoc,
  )
where

import Control.Monad
import Data.Char

main :: IO ()
main = putStrLn "Hello world"

reverseWords' :: IO ()
reverseWords' = do
  line <- getLine
  if null line
    then return ()
    else do
      putStrLn $ reverseWords line
      reverseWords'

reverseWords :: String -> String
reverseWords = unwords . map reverse . words

testWhenMonad :: IO ()
testWhenMonad = do
  input <- getLine
  when (input == "test") $ do
    putStrLn input

-- without sequence
-- ff = do
--   a <- getLine
--   b <- getLine
--   c <- getLine
--   print [a, b, c]
--

testSequence :: IO ()
testSequence = do
  rs <- sequence [getLine, getLine, getLine]
  print rs

-- mapM print [1,2,3] & mapM_ print [1,2,3]

testForever :: IO b
testForever = forever $ do
  putStr "Input: "
  l <- getLine
  putStrLn $ map toUpper l

colorAssoc :: IO [()]
colorAssoc = do
  colors <-
    forM
      [1, 2, 3, 4]
      ( \a -> do
          putStrLn $
            "Collor association for number"
              ++ show a
              ++ "?"
          color <- getLine
          return color
      )
  putStrLn "Colors association for 1,2,3 and 4: "
  mapM putStrLn colors
