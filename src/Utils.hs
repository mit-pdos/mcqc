module Utils where
import Data.Char

capitalised [] = []
capitalised x = toUpper (head x) : map toLower (tail x)

