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

mkTemplateLine :: [CType] -> Doc ann
mkTemplateLine argsT =
    if null templates then mempty else "template<" <> commatize templates <> ">" <> line
    where getTemplates t = take (getMaxVaridx t) ['T'..'Z']
          templates = [ "class" <+> pretty a | a <- L.nub $ concatMap getTemplates argsT]

instance Pretty CDecl where
  pretty CDFunc   { _fd = CDef { .. }, .. } =
          mkTemplateLine (_ty:map (view ty) _fargs)
          <> pretty _ty <+> pretty _nm <> "(" <> (commatize . map pretty $ _fargs) <> ") {"
          <> line <> (tab . pretty) _fbody
          <> line <> "}"
  pretty CDType   { _td = CDef { .. }, .. } =
          mkTemplateLine [_ty]
          <> "using" <+> pretty _nm <+> "=" <+> pretty _ty <> ";"
  pretty CDStruct { _fields = [], ..} = "struct" <+> pretty _sn <+> "{};"
  pretty CDStruct { .. } =
          mkTemplateLine (map (view ty) _fields)
          <> "struct" <+> pretty _sn <+> "{"
          <> line <> (tab . vcat . map (\x -> pretty x <> ";") $ _fields)
          <> line <> (tab $ pretty _sn <> "(" <> (commatize . map pretty $ _fields) <> ") {")
          <> line <> (tab . tab . vcat . map (\x -> "this->" <> toNm x <+> "=" <+> toNm x <> ";") $ _fields)
          <> line <> (tab $ "};")
          <> line <> "};"
    where toNm = pretty . _nm
  pretty CDEmpty {} = mempty
  pretty CDInd { _id = CDef { .. }, .. } = error $ "Inductive declaration " ++ show _nm ++ " was not expanded"
  pretty e = error $ "Unhandled declaration " ++ show e

