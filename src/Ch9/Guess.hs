module Ch9.Guess (run) where

import Control.Monad
import System.Random (StdGen, getStdGen, randomR)

run :: IO ()
run = do
  gen <- getStdGen
  askForNumber gen

askForNumber :: StdGen -> IO ()
askForNumber gen = do
  let (randNumber, newGen) = randomR (1, 10) gen :: (Int, StdGen)
  putStr "Try guess my number 1 - 10"
  numberString <- getLine
  unless (null numberString) $ do
    let number = read numberString
    if randNumber == number
      then putStrLn "Correct!"
      else
        putStrLn $
          "Sorry, correct answer is:"
            ++ show
              randNumber
    askForNumber newGen
