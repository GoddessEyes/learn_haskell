module Ch9.DivTwoNum (printQuotients) where

printQuotients :: Integer -> Integer -> IO()
printQuotients a b = do
  print $ a `div` b
