import Fib
import Prelude


-- fib :: Int -> Int
-- fib x | x < 2 = 1
--       | otherwise = fib (x - 2) + fib (x - 1)

main :: IO()
main = putStrLn $ show (fib 36)
