{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
module CIR.Decl where
import CIR.Expr
import Common.Pretty
import Common.Utils
import GHC.Generics
import Control.Lens
import Data.Aeson
import Data.Text (Text)
import Data.Aeson.Encode.Pretty
import Data.Text.Prettyprint.Doc
import qualified Data.ByteString.Lazy.Char8            as B
import qualified Data.List                             as L

-- Global scope C++ definitions,
data CDecl =
    CDFunc     { _fd :: CDef, _fargs :: [CDef], _fbody :: CExpr }
    | CDType   { _td :: CDef }
    | CDExpr   { _en :: Text, _expr :: CExpr }
    | CDInd    { _id :: CDef, _ictors :: [(Text, CType)] }
    | CDStruct { _sn :: Text, _fields :: [CDef] }
    | CDEmpty  {}
  deriving (Eq, Generic, ToJSON)

instance Show CDecl where
    show = B.unpack . encodePretty

-- Generate Lens code
makeLenses ''CDecl

instance Pretty CDecl where
  pretty CDFunc   { _fd = CDef { .. }, .. } =
        (if null templates then mempty else "template<" <> commatize templates <> ">")
          <> line <> pretty _ty <+> pretty _nm <> "(" <> (commatize . map pretty $ _fargs) <> ") {"
          <> line <> (tab . pretty) _fbody
          <> line <> "}"
    where getTemplates t = take (getMaxVaridx t) ['T'..'Z']
          templates = [ "class" <+> pretty a | a <- L.nub $ getTemplates _ty ++ (concatMap (getTemplates . view ty) _fargs)]
  pretty CDType   { _td = CDef { .. }, .. } = "using" <+> pretty _nm <+> "=" <+> pretty _ty <> ";"
  pretty CDStruct { _fields = [], ..} = "struct" <+> pretty _sn <+> "{};"
  pretty CDStruct { .. } =  "struct" <+> pretty _sn <+> "{"
          <> line <> (tab . vcat . map (\x -> pretty x <> ";") $ _fields)
          <> line <> pretty _sn <> "(" <> (commatize . map pretty $ _fields) <> ") {"
          <> line <> (tab . vcat . map (\x -> "this->" <> pretty x <+> "=" <+> pretty x <> ";") $ _fields)
          <> line <> "};"
  pretty CDEmpty {} = mempty
  pretty CDInd { _id = CDef { .. }, .. } = error $ "Inductive declaration " ++ show _nm ++ " was not expanded"
  pretty e = error $ "Unhandled declaration " ++ show e

