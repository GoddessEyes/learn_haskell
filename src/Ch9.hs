module Ch9
  ( removeTodo,
    main,
    addTodo,
    respondPalindrome,
    testRandom,
    threeCoins,
  )
where

import Data.List
import System.Directory
import System.IO
import System.Random

-- main = do
--   contents <- getContents
--   putStrLn $ shortLinesOnly contents

main :: IO ()
main = interact shortLinesOnly

addTodo :: IO ()
addTodo = do
  todoItem <- getLine
  appendFile "todo.txt" (todoItem ++ "\n")

removeTodo :: IO ()
removeTodo = do
  contents <- readFile "todo.txt"
  let todoTasks = lines contents
      numberedTasks = zipWith (\n line -> show n ++ " - " ++ line) [0 ..] todoTasks

  putStrLn "Your tasks: "
  mapM_ putStrLn numberedTasks
  putStrLn "Remove number:"
  numberString <- getLine
  let number = read numberString
      newTodoItems = unlines $ delete (todoTasks !! number) todoTasks
  (tempName, tempHandle) <- openTempFile "." "temp"
  hPutStr tempHandle newTodoItems
  hClose tempHandle
  removeFile "todo.txt"
  renameFile tempName "todo.txt"

shortLinesOnly :: String -> String
shortLinesOnly = unlines . filter (\line -> length line < 15) . lines

respondPalindrome :: String -> String
respondPalindrome =
  unlines
    . map (\xs -> if isPal xs then "Palindrome" else "Not palindrome")
    . lines

isPal :: (Eq a) => [a] -> Bool
isPal xs = xs == reverse xs

testRandom :: Int -> (Int, StdGen)
testRandom num =
  random (mkStdGen num) :: (Int, StdGen)

threeCoins :: StdGen -> (Bool, Bool, Bool)
threeCoins gen =
  let (firstCoin, newGen) = random gen
      (secondCoin, newGen') = random newGen
      (thirdCoin, _) = random newGen'
   in (firstCoin, secondCoin, thirdCoin)
