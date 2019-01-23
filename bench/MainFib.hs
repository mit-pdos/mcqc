{-# LANGUAGE StandaloneDeriving #-}
module Main where
import Fib
import Datatypes
import System.Environment

main :: IO()
main = do
  arg <- head <$> getArgs
  let test = read arg :: Int
  putStrLn $ "Fibonacci"
  putStrLn . show . fib $ test

