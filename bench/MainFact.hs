{-# LANGUAGE StandaloneDeriving #-}
module Main where
import Fact
import Datatypes
import System.Environment

main :: IO()
main = do
  arg <- head <$> getArgs
  let test = read arg :: Int
  putStrLn $ "Factorial"
  putStrLn . show . fact $ test

