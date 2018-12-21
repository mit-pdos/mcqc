{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
module CIR.Decl where
import CIR.Expr hiding (omapM)
import Common.Pretty
import GHC.Generics
import Types.Context
import Data.Aeson
import Data.MonoTraversable
import Data.Text (Text)
import Data.Aeson.Encode.Pretty
import Data.Text.Prettyprint.Doc
import qualified Data.ByteString.Lazy.Char8 as B
import qualified Data.List                  as L
import qualified Data.Map                   as M

-- Global scope C++ definitions,
data CDecl =
    CDFunc     { _fd :: CDef, _fargs :: [CDef], _fbody :: CExpr }
    | CDType   { _td :: CDef }
    | CDExpr   { _en :: Text, _expr :: CExpr }
    | CDInd    { _id :: CDef, _ictors :: [(Text, CType)] }
    | CDStruct { _sn :: Text, _fields :: [CDef] }
    | CDSeq    { _left :: CDecl, _right :: CDecl }
    | CDEmpty  {}
  deriving (Eq, Generic, ToJSON)

type instance Element CDecl = CDecl

instance Show CDecl where
    show = B.unpack . encodePretty

instance Semigroup CDecl where
    (<>) = CDSeq

instance Monoid CDecl where
    mappend = (<>)
    mempty  = CDEmpty

instance MonoFunctor CDecl where
    omap f CDSeq { .. } = f _left <> f _right
    omap f CDEmpty = mempty

instance MonoFoldable CDecl where
    ofoldMap f = ofoldr (mappend . f) mempty

    ofoldr f b CDEmpty = b
    ofoldr f b CDSeq { .. } = f _left (ofoldr f b _right)
    ofoldr f b d = f d b

    ofoldl' f b CDSeq { .. } = ofoldl' f (f b _left) _right
    ofoldl' f b CDEmpty = b
    ofoldl' f b d = f b d

    otoList CDSeq { .. } = _left:otoList _right
    otoList CDEmpty      = []
    otoList d            = [d]

    onull CDEmpty = True
    onull d = False

    olength = length . otoList
    ofoldr1Ex f = ofoldr1Ex f . otoList
    ofoldl1Ex' f = ofoldl1Ex' f . otoList

instance MonoTraversable CDecl where
    otraverse f CDSeq { .. } = CDSeq <$> otraverse f _left <*> otraverse f _right
    otraverse f CDEmpty = pure CDEmpty
    otraverse f d = f d

-- Has a name
instance Nameful CDecl where
    getname CDFunc   { .. } = _nm _fd
    getname CDType   { .. } = _nm _td
    getname CDExpr   { .. } = _en
    getname CDInd    { .. } = _nm _id
    getname CDStruct { .. } = _sn
    getname CDEmpty  {} = ""

-- Has a type
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

-- Pretty print the template line
mkTemplateLine :: [CType] -> Doc ann
mkTemplateLine argsT =
    if null templates then mempty else "template<" <> commatize templates <> ">" <> line
    where getTemplates t = take (getMaxVaridx t) ['T'..'Z']
          templates = [ "class" <+> pretty a | a <- L.nub $ concatMap getTemplates argsT]

-- Pretty print a declaration
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
  pretty CDSeq { .. } = vsep [pretty _left, pretty _right]
  pretty e = error $ "Unhandled declaration " ++ show e

