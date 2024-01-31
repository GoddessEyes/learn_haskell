main = do
  contents <- getContents
  putStrLn $ shortLinesOnly contents

shortLinesOnly :: String -> String
shortLinesOnly = unlines . filter (\line -> length line < 15) . lines
