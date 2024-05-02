module Ch13.Ch13 (applyMaybe) where

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
