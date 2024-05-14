module Ch13.Ch13 (applyMaybe, banana, landLeft, landRight, foo, pierRoutine, justFirst, wopwop, listOfTuples) where

-- (\x -> Just $ 1 + x) 1

applyMaybe :: Maybe a -> (a -> Maybe b) -> Maybe b
applyMaybe Nothing _ = Nothing
applyMaybe (Just x) f = f x

-- Just 3 `applyMaybe` \x -> Just (x+1)

-- Just 9 >>= \x -> return $ x * 10

type Birds = Int

type Pole = (Birds, Birds)

landLeft :: Birds -> Pole -> Maybe Pole
landLeft n (left, right)
  | abs ((left + n) - right) < 4 = Just (left + n, right)
  | otherwise = Nothing

landRight :: Birds -> Pole -> Maybe Pole
landRight n (left, right)
  | abs (left - (right + n)) < 4 = Just (left, right + n)
  | otherwise = Nothing

-- λ> return (0, 0) >>= landRight 2 >>= landLeft 12 >>= landRight 2
--Nothing
--λ> return (0, 0) >>= landRight 2 >>= landLeft 2 >>= landRight 2
--Just (2,4)

banana :: Pole -> Maybe Pole
banana _ = Nothing

-- λ> return (0, 0) >>= landRight 2 >>= landLeft 2 >>= banana >>= landRight 2
--Nothing
-- return (0, 0) >>= landRight 2 >>= landLeft 2 >> Nothing >>= landRight 2
--Nothing

foo :: Maybe String
foo = do
  x <- Just 3
  y <- Just "!"
  Just (show x ++ y)

pierRoutine :: Maybe Pole
pierRoutine = do
  let start = (0, 0)
  first <- landLeft 2 start
  --  Nothing
  second <- landRight 2 first
  landLeft 1 second

justFirst :: Maybe Char
justFirst = do
  (x : _) <- Just "Hello"
  return x

wopwop :: Maybe Char
wopwop = do
  (x : _) <- Just ""
  return x

-- [3,4,5] >>= \x -> [x, -x]
-- [] >>= \_ -> [1, 2, 3]
listOfTuples :: [(Int, String)]
listOfTuples = do
  n <- [1, 2]
  ch <- ["a", "b"]
  return (n, ch)
