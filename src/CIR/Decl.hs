{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
module CIR.Decl where
import CIR.Expr
import Common.Pretty
import GHC.Generics
import Types.Context
import Data.Aeson
import Data.Text (Text)
import Data.Aeson.Encode.Pretty
import Data.Text.Prettyprint.Doc
import qualified Data.ByteString.Lazy.Char8 as B
import qualified Data.List                  as L
import qualified Control.Lens               as Lens
import qualified Data.Map                   as M

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
Lens.makeLenses ''CDecl

-- Get declaration name
getname :: CDecl -> Text
getname CDFunc   { .. } = _nm _fd
getname CDType   { .. } = _nm _td
getname CDExpr   { .. } = _en
getname CDInd    { .. } = _nm _id
getname CDStruct { .. } = _sn
getname CDEmpty  {} = ""

-- Pretty print the template line
mkTemplateLine :: [CType] -> Doc ann
mkTemplateLine argsT =
    if null templates then mempty else "template<" <> commatize templates <> ">" <> line
    where getTemplates t = take (getMaxVaridx t) ['T'..'Z']
          templates = [ "class" <+> pretty a | a <- L.nub $ concatMap getTemplates argsT]

instance Typeful CDecl where
    getincludes CDEmpty  {}     = []
    getincludes CDType   { .. } = getincludes _td
    getincludes CDInd    { .. } = "variant" : getincludes _id ++ concatMap (getincludes . snd) _ictors
    getincludes CDFunc   { .. } = getincludes _fd ++ concatMap getincludes _fargs ++ getincludes _fbody
    getincludes CDStruct { .. } = concatMap getincludes _fields
    getincludes CDExpr   { .. } = getincludes _expr

    unify ctx t CDType   { .. } = CDType $ unify ctx t _td
    unify ctx t CDExpr   { .. } = CDExpr _en $ unify ctx t _expr
    unify ctx t CDFunc   { .. } = CDFunc (unify ctx t _fd) _fargs $ unify ctx t _fbody
    unify ctx t CDStruct { .. } = CDStruct _sn $ map (unify ctx t) _fields
    unify _ _  a = a

    gettype CDType { .. } = gettype _td
    gettype CDExpr { .. } = gettype _expr
    gettype CDFunc { .. } = CTFunc (_ty _fd) (map gettype _fargs)
    gettype _             = CTUndef

    addctx ctx d@CDFunc { .. } = mergeCtx ctx $ M.singleton (_nm _fd) (gettype d)
    addctx ctx CDType { .. } = addctx ctx _td
    addctx ctx CDExpr { .. } = mergeCtx ctx $ M.singleton _en (gettype _expr)
    addctx ctx _ = ctx

    getMaxVaridx = getMaxVaridx . gettype

instance Pretty CDecl where
  pretty CDFunc   { _fd = CDef { .. }, .. } =
          mkTemplateLine (_ty:map gettype _fargs)
          <> pretty _ty <+> pretty _nm <> "(" <> (commatize . map pretty $ _fargs) <> ") {"
          <> line <> (tab . pretty) _fbody
          <> line <> "}" <> line
  pretty CDType   { _td = CDef { .. }, .. } =
          mkTemplateLine [_ty]
          <> "using" <+> pretty _nm <+> "=" <+> pretty _ty <> ";" <> line
  pretty CDStruct { _fields = [], ..} = "struct" <+> pretty _sn <+> "{};"
  pretty CDStruct { .. } =
          mkTemplateLine (map gettype _fields)
          <> "struct" <+> pretty _sn <+> "{"
          <> line <> (tab . vcat . map (\x -> pretty x <> ";") $ _fields)
          <> line <> (tab $ pretty _sn <> "(" <> (commatize . map pretty $ _fields) <> ") {")
          <> line <> (tab . tab . vcat . map (\x -> "this->" <> toNm x <+> "=" <+> toNm x <> ";") $ _fields)
          <> line <> (tab $ "};")
          <> line <> "};" <> line
    where toNm = pretty . _nm
  pretty CDEmpty {} = mempty
  pretty CDInd { _id = CDef { .. }, .. } = error $ "Inductive declaration " ++ show _nm ++ " was not expanded"
  pretty e = error $ "Unhandled declaration " ++ show e

