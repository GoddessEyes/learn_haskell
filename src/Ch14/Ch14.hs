module Ch14.Ch14 (isBigGang, addDrink, applyLog) where

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
