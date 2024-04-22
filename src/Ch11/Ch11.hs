module Ch11.Ch11 (main) where

main :: IO ()
main = do
  line <- fmap reverse getLine
  putStrLn $ "Reversed string: " ++ line

-- fmap (replicate 3) [1, 2, 3, 4]
-- fmap (replicate 3) Just 3
-- fmap (replicate 3) Right 3
-- fmap (replicate 3) Nothing
-- fmap (replicate 3) Left "Whoops"
