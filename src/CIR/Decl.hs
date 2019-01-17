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
    | CDStruct { _sn :: Text, _fields :: [CDef], _nfree :: Int }
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
    omap _ CDEmpty = mempty
    omap _ d = d

instance MonoFoldable CDecl where
    ofoldMap f = ofoldr (mappend . f) mempty

    ofoldr _ b CDEmpty = b
    ofoldr f b CDSeq { .. } = f _left (ofoldr f b _right)
    ofoldr f b d = f d b

    ofoldl' f b CDSeq { .. } = ofoldl' f (f b _left) _right
    ofoldl' _ b CDEmpty = b
    ofoldl' f b d = f b d

    otoList CDSeq { .. } = _left:otoList _right
    otoList CDEmpty      = []
    otoList d            = [d]

    onull CDEmpty = True
    onull _ = False

    olength = length . otoList
    ofoldr1Ex f = ofoldr1Ex f . otoList
    ofoldl1Ex' f = ofoldl1Ex' f . otoList

instance MonoTraversable CDecl where
    otraverse f CDSeq { .. } = CDSeq <$> otraverse f _left <*> otraverse f _right
    otraverse _ CDEmpty = pure CDEmpty
    otraverse f d = f d

-- Has a name
instance Nameful CDecl where
    getname CDFunc   { .. } = _nm _fd
    getname CDType   { .. } = _nm _td
    getname CDStruct { .. } = _sn
    getname CDEmpty  {} = ""
    getname CDSeq    { .. } = getname _left

-- Has a type
instance Typeful CDecl where
    getincludes CDEmpty  {}     = []
    getincludes CDType   { .. } = getincludes _td
    getincludes CDFunc   { .. } = getincludes _fd ++ concatMap getincludes _fargs ++ getincludes _fbody
    getincludes CDStruct { .. } = concatMap getincludes _fields
    getincludes CDSeq    { .. } = getincludes _left ++ getincludes _right

    unify ctx t CDType   { .. } = CDType $ unify ctx t _td
    unify ctx t CDFunc   { .. } = CDFunc (unify ctx t _fd) _fargs $ unify ctx t _fbody
    unify ctx t CDStruct { .. } = CDStruct _sn (map (unify ctx t) _fields) _nfree
    unify ctx t CDSeq    { .. } = CDSeq (unify ctx t _left) (unify ctx t _right)
    unify _ _  a = a

    gettype CDType { .. } = gettype _td
    gettype CDFunc { .. } = CTFunc (_ty _fd) (map gettype _fargs)
    gettype _             = CTAuto

    addctx ctx CDFunc { _fd = CDef { _nm = "match" } } = ctx
    addctx ctx d@CDFunc { .. } = mergeCtx ctx $ M.singleton (_nm _fd) (gettype d)
    addctx ctx CDType { _td = CDef { _ty = CTExpr { _tbase = "std::variant", ..  }, .. } }
        | freedom > 0 = mergeCtx ctx $ M.fromList . map exprmaker $ _tins
        | otherwise = mergeCtx ctx $ M.fromList . map basemaker $ _tins
        where freedom = maximum . map getMaxVaridx $ _tins
              exprmaker n = (getname n, CTExpr _nm [CTFree i | i <- [1..freedom]])
              basemaker n = (getname n, CTBase _nm)
    addctx ctx CDType { .. } = addctx ctx _td
    addctx ctx CDSeq  { .. } = ctx `addctx` _left `addctx` _right
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
  pretty CDStruct { _fields = [], .. } =
          mkTemplateLine [CTFree i | i <- [1.._nfree]]
          <> "struct" <+> pretty _sn <+> "{};"
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
  pretty CDSeq { .. } = vcat [pretty _left, pretty _right]
  pretty e = error $ "Unhandled declaration " ++ show e

