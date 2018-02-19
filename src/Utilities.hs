module Utilities where
import System.Directory (doesFileExist, doesDirectoryExist)
import Data.Bool

check :: FilePath -> IO Bool
check s = do
  result <- doesFileExist s
  putStrLn $
    s ++
    if result
      then " loaded"
      else " does not exist"

  return result


