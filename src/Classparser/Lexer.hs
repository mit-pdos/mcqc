{-# LANGUAGE FlexibleContexts, OverloadedStrings #-}
module Classparser.Lexer where
import Text.Regex
import Types.Inference
import Text.Regex.Base
import Data.Text (Text)
import Data.List.Split
import qualified Data.Text         as T
import qualified Data.Map.Strict   as M
import qualified Data.Array        as A

-- Take a typeclass definition file and return type context from the constructors
getCtors :: String -> Context Text
getCtors  body =
    let lines = re body in
    M.fromList . map (toCtor . tokenize) $ lines
    where re = map (fst . head . A.elems) . matchAllText (mkRegex "[A-Za-z]+: .*;")
          tokenize = map removeWhite . split (dropDelims . dropBlanks $ oneOf ":;")
          toCtor [a,b] = (a, splitOnArrow b)
          toCtor a = error $ "Error parsing constructor " ++ (show . head $ a)
          removeWhite =  T.strip . T.pack
          splitOnArrow = map removeWhite . split (dropDelims $ onSublist "->") . T.unpack

-- Get instance (free types, bound types)
-- One instance declaration per translation unit supported for now
getPlugs :: String -> ([Text], [Text])
getPlugs body =
    case re body of
        ([])      -> ([],[])
        (first:_) -> splitOnClass . wrap . tokenize $ first
    where re = map (fst . head . A.elems) . matchAllText (mkRegex "Instance [A-Za-z]+ (.*) :=")
          tokenize = map T.pack . split (dropDelims . dropBlanks $ oneOf "}{: ")
          wrap ("Instance":_:ts) = parenthesize . init $ ts
          wrap _ = mempty
          parenthesize (a:b:ts)
              | "(" `T.isPrefixOf` a &&
                ")" `T.isSuffixOf` b = (T.concat [a, " ", b]):parenthesize ts
              | otherwise = a:(parenthesize (b:ts))
          parenthesize o = o
          splitOnClass l = case splitWhen (T.isPrefixOf "Native") l of
                               ([free, bound]) -> (free, bound)
                               (_) -> ([], [])
-- Get class free variables
getAbstractors :: String -> [Text]
getAbstractors body =
    case re body of
        ([])      -> []
        (first:_) -> wrap . tokenize $ first
    where re = map (fst . head . A.elems) . matchAllText (mkRegex "Class [A-Za-z]+ (.*) :=")
          tokenize = map T.pack . split (dropDelims . dropBlanks $ oneOf " ")
          wrap ("Class":_:ts) = init ts
          wrap _ = error "Error parsing typeclass declaration"

