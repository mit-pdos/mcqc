{-# LANGUAGE StandaloneDeriving #-}
module Main where
import Rev
import Datatypes
import System.Environment

deriving instance Show a => Show (Coq_list a)

main :: IO()
main = do
  arg <- head <$> getArgs
  let test = series (read arg :: Int)
  putStrLn $ "Reversed"
  putStrLn . show . rev $ test

