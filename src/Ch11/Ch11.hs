module Ch11.Ch11 (main, myAction) where

  
main :: IO ()
main = do
  line <- fmap reverse getLine
  putStrLn $ "Reversed string: " ++ line

-- fmap (replicate 3) [1, 2, 3, 4]
-- fmap (replicate 3) Just 3
-- fmap (replicate 3) Right 3
-- fmap (replicate 3) Nothing
-- fmap (replicate 3) Left "Whoops"

data CMaybe a = CNothing | CJust Int a deriving (Show)

instance Functor CMaybe where
  fmap _ CNothing = CNothing
  fmap f (CJust counter x) = CJust (counter + 1) (f x)

--λ> fmap id (CJust 0 "ha-ha-ha") <- передали CJust 0
--CJust 1 "ha-ha-ha" <- получили Cjust 1. Нарушение fmap id = id

-- Just (+3) <*> Just 9
-- pure (+3) <*> Just 3
-- pure (+) <*> Just 3 <*> Just 5 <-- частичное применение Just (+) -> Just (+3) -> Just (3 + 5) -> Just 8

-- λ> [(*0), (+100), (*2)] <*> [1, 2, 3]
-- λ> [0,0,0,101,102,103,2,4,6]

myAction :: IO String
myAction = (++) <$> getLine <*> getLine
