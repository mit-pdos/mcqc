module Main where
import FibHs

overflows :: Int
overflows = 123124213123123123 + 232141*4235425234234

main :: IO()
main = putStrLn . show . fib $ 35
-- putStrLn . show $ overflows
