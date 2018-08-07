module Main where
import System.Process

main:: IO()
main = do
    fd <- runCommand "lit test/"
    return ()
