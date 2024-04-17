module Ch9.Exceptions (main) where

import Control.Exception
import System.Environment

mainAction :: [String] -> IO ()
mainAction args = do
  let (a, b) = params args
  printQuotients a b

printQuotients :: Integer -> Integer -> IO ()
printQuotients a b = do
  print $ a `div` b
  print $ b `div` a

params :: [String] -> (Integer, Integer)
params [a, b] = (read a, read b)

handleArith :: ArithException -> IO ()
handleArith _ = putStrLn "Div by zero"

handleArgs :: PatternMatchFail -> IO ()
handleArgs _ = putStrLn "Unexpected args count"

handleOther :: SomeException -> IO ()
handleOther e = putStrLn $ "Unexpected error: " ++ show e

main :: IO ()
main = do
  args <- getArgs
  mainAction args `catches` [Handler handleArith, Handler handleArgs, Handler handleOther]
  putStrLn "Exit"

-- ghc -main-is Ch9.Exceptions src/Ch9/Exceptions.hs
-- ./src/Ch9/Exceptions 20 1
