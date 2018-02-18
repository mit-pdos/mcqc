module Main where
import System.Environment

main :: IO ()

main = do  
    args <- getArgs
    mapM_ putStrLn args
