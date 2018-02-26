module Utils where
import Data.Char

capitalised [] = []
capitalised x = toUpper (head x) : map toLower (tail x)

lowercase :: [Char] -> [Char]
lowercase [] = []
lowercase (h:ts) = (toLower h):(lowercase ts)
