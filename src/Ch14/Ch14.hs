module Ch14.Ch14 (isBigGang, addDrink, applyLog, multWithLog, multWithLog', gcd') where

import Control.Monad.Writer
import Data.Monoid

isBigGang :: Int -> (Bool, String)
isBigGang x = (x > 9, "Check size with 9")

type Food = String

type Price = Sum Int

applyLog :: (Monoid m) => (a, m) -> (a -> (b, m)) -> (b, m)
applyLog (x, log') f = let (y, newLog) = f x in (y, log' `mappend` newLog)

addDrink :: Food -> (Food, Price)
addDrink "Beans" = ("Milk", Sum 25)
addDrink "Meat" = ("Whiskey", Sum 99)
addDrink _ = ("Beer", Sum 30)

-- ("Beans", Sum 10) `applyLog` addDrink
-- runWriter (return 3 :: Writer Sum Int Int)

logNumber :: Int -> Writer [String] Int
logNumber x = writer (x, ["Got number: " ++ show x])

multWithLog :: Writer [String] Int
multWithLog = do
  a <- logNumber 3
  b <- logNumber 5
  return (a * b)

multWithLog' :: Writer [String] Int
multWithLog' = do
  a <- logNumber 3
  b <- logNumber 5
  tell ["Got it"]
  return (a * b)

gcd' :: Int -> Int -> Writer [String] Int
gcd' a b
  | b == 0 = do
    tell ["Finish: " ++ show a]
    return a
  | otherwise = do
    tell [show a ++ " mod " ++ show b ++ " = " ++ show (a `mod` b)]
    gcd' b (a `mod` b)

-- fst $ runWriter (gcd' 3 8)
-- mapM_ putStrLn $ snd $ runWriter (gcd' 3 8)
