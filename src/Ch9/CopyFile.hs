module Ch9.CopyFile (main) where

import Control.Exception
import qualified Data.ByteString.Lazy as B
import System.Directory
import System.Environment
import System.IO

main :: IO ()
main = do
  (fileName1 : fileName2 : _) <- getArgs
  copy fileName1 fileName2

copy :: FilePath -> FilePath -> IO ()
copy source dest = do
  contents <- B.readFile source
  bracketOnError
    (openTempFile "." "temp")
    ( \(tempName, tempHandle) -> do
        hClose tempHandle
        removeFile tempName
    )
    ( \(tempName, tempHandle) -> do
        B.hPutStr tempHandle contents
        hClose tempHandle
        renameFile tempName dest
    )

-- ghc -main-is Ch9.CopyFile src/Ch9/CopyFile.hs
-- ./src/Ch9/CopyFile src/Ch9/bart.txt src/Ch9/bort.txt
