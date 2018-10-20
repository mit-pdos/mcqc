{-# LANGUAGE FlexibleContexts, OverloadedStrings #-}
module Classparser.Regex where
import Text.Regex
import CIR.Expr
import Types.Inference
import Text.Regex.Base
import Data.Text (Text)
import Data.List.Split
import Data.Map.Strict (Map)
import qualified Data.Text         as T
import qualified Data.List         as L
import qualified Data.Map.Strict   as M
import qualified Data.Array        as Array

-- Take a typeclass definition file and return lexical tokens (function, [args])
lexer :: String -> Context Text
lexer body =
    let lines = re body in
    M.fromList . map (toPair . tokenize) $ lines
    where re = map (fst . head . Array.elems) . matchAllText (mkRegex "[A-Za-z]+:.*;")
          tokenize = map removewhite . split (dropDelims . dropBlanks $ oneOf ":;")
          toPair [a,b] = (a, dearrow b)
              where dearrow = map (T.strip . T.pack) . split (dropDelims $ onSublist "->") . T.unpack
          toPair a = error $ "Error parsing method " ++ (show . head $ a)
          removewhite =  T.strip . T.pack

-- Get typeclass name and free variables
getclass :: String -> (Text, [Text])
getclass body =
    let lines = re body in
    wrap . tokenize . head $ lines
    where re = map (fst . head . Array.elems) . matchAllText (mkRegex "Class [A-Za-z]+ (.*) :=")
          tokenize = map T.pack . split (dropDelims . dropBlanks $ oneOf " ")
          wrap ("Class":ts) = wrap ts
          wrap (a:ts) = (a, init ts)
          wrap _ = error "Error parsing typeclass declaration"
