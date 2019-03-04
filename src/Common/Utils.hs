{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
module Common.Utils where
import CIR.Expr
import Codegen.Rewrite
import qualified Data.Text as T
import qualified Data.Char as C
import qualified Data.List as L
import qualified Common.Config as Conf
import Data.Text (Text)
import Common.Pretty
import Data.Word (Word8)
import Debug.Trace
import Data.Text.Prettyprint.Doc

-- Word to Character cast
w2c :: Word8 -> Char
w2c = C.chr . fromIntegral

-- Print warning
warn :: String -> a -> a
warn s = trace ("Warning: " ++ s)

-- Zip names and types to fields
zipf :: [Text] -> [CType] -> [CDef]
zipf = zipWith (\a b -> CDef a b)

-- map across a list of CDefs
mapf :: (Text -> CType -> a) -> [CDef] -> [a]
mapf f = map (\d -> case d of (CDef { .. }) -> f _nm _ty)

-- Give an ord of names to types, useful for making constructors
givenm :: Char -> [CType] -> [CDef]
givenm c = zipf [T.pack [i] | i <- [c..]]

-- Wrap non-base types to a pointer
addPtr :: CType -> CType
addPtr t@CTBase { .. }
    | _base `elem` Conf.base = t
    | otherwise = CTPtr t
addPtr t@CTExpr { .. }
    | _tbase `elem` Conf.base = t
    | otherwise = CTPtr t
addPtr t@CTVar { .. }
    | _vname `elem` Conf.base = t
    | otherwise = CTPtr t
addPtr t = t

-- Pretty print the template line
mkTemplateLine :: [CType] -> Doc ann
mkTemplateLine argsT
    | length (prettytempl nfreevars argsT) > 0 = "template<" <> (commatize . L.nub $ prettytempl nfreevars argsT) <> ">" <> line
    | otherwise = mempty
    where nfreevars = maximum . map getMaxVaridx $ argsT
          mktemplate n = pretty $ stringsFromTo 'T' 'Z' !! n
          isFuncRet n CTFunc { .. } | _fret == CTFree n = True
          isFuncRet _ _ = False
          prettytempl n (CTFunc { _fret = CTFree { .. }, .. }:ts) =
            prettytempl (n+1) (_fins ++ ts) ++
            ["class" <+> mktemplate n,
             "class" <+> mktemplate (_idx-1) <+> "= std::invoke_result_t<"
                <> mktemplate n <> ","
                <+> (commatize . map pretty $ _fins) <> ">"]
          prettytempl n (CTFunc { .. }:ts) = "class" <+> mktemplate n : prettytempl (n+1) (_fins ++ ts)
          prettytempl n (CTFree { .. }:ts)
              | isFuncRet _idx `L.any` ts = prettytempl n ts
              | otherwise = "class" <+> mktemplate (_idx-1) : prettytempl n ts
          prettytempl n (CTExpr { .. }:ts) = prettytempl n (_tins ++ ts)
          prettytempl n (CTPtr  { .. }:ts) = prettytempl n (_inner:ts)
          prettytempl n (_:ts) = prettytempl n ts
          prettytempl _ [] = []
