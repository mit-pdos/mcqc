{-# LANGUAGE DeriveGeneric, OverloadedStrings, RecordWildCards, DeriveAnyClass #-}
module Clang.Namespaces where
import GHC.Generics hiding (Constructor)
import Data.Aeson
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Prettyprint.Doc
import Data.Text.Prettyprint.Doc.Render.Text

-- Top level namespace
data Namespace = Namespace { namespace :: Text, functions :: [ Namespaces ] }
    deriving (Show, Eq, Generic, FromJSON)

-- Function Signature
data Namespaces = Namespaces { name :: Text, typ :: Text, args :: [ Text ] }
    deriving (Show, Eq, Generic, FromJSON)

instance Pretty Namespace where
    pretty Namespace { .. }  = "== "
                               <+> pretty namespace
                               <+> line
                               <+> nest 4 (vcat (map pretty functions))
instance Pretty Namespaces where
    pretty Namespaces { .. } = pretty typ
                            <+> pretty name
                            <+> "("
                            <> vcat (map (\s -> pretty s <> ", ") (safeinit args))
                            <> pretty (safelast args)
                            <> ")"
                                where safeinit [] = []
                                      safeinit l = init l
                                      safelast [] = ""
                                      safelast l = last l

