module Ch9 (removeTodo) where

import Data.List
import System.Directory
import System.IO

-- main = do
--   contents <- getContents
--   putStrLn $ shortLinesOnly contents

main = interact shortLinesOnly

addTodo = do
  todoItem <- getLine
  appendFile "todo.txt" (todoItem ++ "\n")

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

isPal xs = xs == reverse xs
