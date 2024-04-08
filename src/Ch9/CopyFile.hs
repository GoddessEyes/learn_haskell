import Control.Exception
import qualified Data.ByteString as S
import qualified Data.ByteString.Lazy as B
import Data.List
import System.Directory
import System.Environment
import System.IO

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

-- ./src/Ch9/CopyFile src/Ch9/bart.txt src/Ch9/bort.txt
