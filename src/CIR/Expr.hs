{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module CIR.Expr where
import GHC.Generics
import Control.Lens
import Data.Aeson
import Data.MonoTraversable
import Common.Pretty
import Data.Text.Prettyprint.Doc
import Data.Text (Text)
import qualified Data.Text as T

-- C++ Typed names
data CDef = CDef { _nm :: Text, _ty :: CType }
    deriving (Show, Eq, Generic, ToJSON)

-- C++ Types
data CType =
    CTFunc    { _fret :: CType, _fins :: [CType] }
    | CTExpr  { _tbase :: Text, _tins :: [CType] }
    | CTVar   { _vname :: Text, _vargs :: [CExpr] }
    | CTBase  { _base :: Text }
    | CTPtr   { _inner :: CType }
    | CTFree  { _idx :: Int }
    | CTAuto  {}
    | CTUndef {} -- Should never output this type, means type inference failed
    deriving (Show, Eq, Generic, ToJSON)

-- C++ Expressions
data CExpr =
          -- High level C++ expressions
            CExprLambda { _lds :: [CDef], _lbody :: CExpr }
          | CExprCall   { _fname :: Text, _fparams :: [CExpr] }
          -- Continuations
          | CExprSeq    { _left :: CExpr, _right :: CExpr }
          -- C++ statament
          | CExprStmt   { _sd :: CDef, _sbody :: CExpr }
          -- Reduced forms
          | CExprVar    { _var :: Text }
          | CExprStr    { _str :: Text }
          | CExprNat    { _nat :: Int }
          | CExprBool   { _bool :: Bool }
          | CExprList   { _etype :: CType, _elems :: [CExpr] }
          | CExprOption { _otype :: CType, _val :: Maybe CExpr }
          | CExprTuple  { _items :: [CExpr] }
    deriving (Show, Eq, Generic, ToJSON)

-- Compatible with Element a = a
type instance Element CExpr = CExpr
type instance Element CType = CType
type instance Element CDef  = CDef

-- CTypes are monomorphic functors
instance MonoFunctor CType where
    omap f   CTFunc { .. } = CTFunc (f _fret) $ fmap f _fins
    omap f   CTExpr { .. } = CTExpr _tbase $ fmap f _tins
    omap f   CTPtr  { .. } = CTPtr $ f _inner
    omap _   CTVar  { .. } = error $ "Type:var with CExpr subterm cannot be traversed " ++ show _vname
    omap _   other         = other

-- CExpr are monomorphic functors
instance MonoFunctor CExpr where
    omap f   CExprCall   { .. } = CExprCall _fname $ fmap f _fparams
    omap f   CExprStmt   { .. } = CExprStmt _sd $ f _sbody
    omap f   CExprSeq    { .. } = CExprSeq (f _left) (f _right)
    omap f   CExprTuple  { .. } = CExprTuple $ fmap f _items
    omap f   CExprList   { .. } = CExprList _etype $ fmap f _elems
    omap f   CExprOption { .. } = CExprOption _otype $ fmap f _val
    omap f   CExprLambda { .. } = CExprLambda _lds $ f _lbody
    -- If it doesn't match anything, then it's a normal form, ignore
    omap _   other              = other

-- Foldable is not possible as CExpr cannot be empty, FunctorM will do
class MonoFunctorM mono where
    omapM :: Monad m => (Element mono -> m (Element mono)) -> mono -> m mono

instance MonoFunctorM CExpr where
    omapM f   CExprCall   { .. } = mapM f _fparams >>= \ps -> return $ CExprCall _fname ps
    omapM f   CExprStmt   { .. } = f _sbody >>= \b -> return $ CExprStmt _sd b
    omapM f   CExprLambda { .. } = f _lbody >>= \b -> return $ CExprLambda _lds b
    omapM f   CExprSeq    { .. } = do { l <- f _left; r <- f _right; return $ CExprSeq l r }
    omapM f   CExprTuple  { .. } = mapM f _items >>= \items -> return $ CExprTuple items
    omapM f   CExprList   { .. } = mapM f _elems >>= \elems -> return $ CExprList _etype elems
    omapM f   CExprOption { _val = Just a, .. } = f a >>= \b -> return $ CExprOption _otype (Just b)
    omapM _   CExprOption { _val = Nothing, .. } = return $ CExprOption _otype Nothing
    -- If it doesn't match anything, then it's a normal form, ignore
    omapM _   other              = return other

-- Utility functions
-- Convert sequence to list of expressions
seqToList :: CExpr -> [CExpr]
seqToList CExprSeq { .. } = _left:seqToList _right
seqToList other           = [other]

-- Convert a list of expressions to a sequence
listToSeq :: [CExpr] -> CExpr
listToSeq []     = error "Empty sequence list given, unable to convert to expression"
listToSeq [a]    = a
listToSeq (a:ts) = CExprSeq a $ listToSeq ts

-- Pretty printer
instance Pretty CDef where
    pretty CDef { .. } = pretty _ty <+> pretty _nm

instance Pretty CType where
  pretty CTFunc  { .. } = group $ pretty _fret <> (parens . commatize $ map pretty _fins)
  pretty CTExpr  { .. } = pretty _tbase <> "<" <> commatize (map pretty _tins) <> ">"
  pretty CTBase  { .. } = pretty _base
  -- Use template letters starting at T as is custom in C++
  pretty CTFree  { .. } = pretty $ ['T'..'Z'] !! (_idx - 1)
  pretty CTAuto  {}     = "auto" :: Doc ann
  pretty CTUndef {}     = error "Undefined type found, inference failed" -- error "Undef type found in the end, internal error"
  pretty CTPtr   { .. } = "std::shared_ptr<" <> pretty _inner <> ">"

instance Pretty CExpr where
  pretty CExprLambda { _lbody = s@CExprSeq { .. }, .. } =
                            group $ "[=](" <> (commatize . map pretty $ _lds) <> ") {"
                            <> line
                            <> tab (pretty s)
                            <> line
                            <> "}"
  pretty CExprLambda { .. } =
                            group $ "[=](" <> (commatize . map pretty $ _lds) <> ") {"
                            <+> "return" <+> pretty _lbody <> ";"
                            <+> "}"
  pretty CExprCall   { _fname = "return", _fparams = [a] } = "return" <+> pretty a <> ";"
  pretty CExprCall   { _fname = "eqb", _fparams = [a, b] } = pretty a <+> "==" <+> pretty b
  pretty CExprCall   { _fname = "ltb", _fparams = [a, b] } = pretty a <+> "<"  <+> pretty b
  pretty CExprCall   { _fname = "leb", _fparams = [a, b] } = pretty a <+> "<=" <+> pretty b
  pretty CExprCall   { _fname = "match", .. } = "match" <> (parens . breakcommatize $ _fparams)
  pretty CExprCall   { .. } = pretty _fname <> (parens . commatize $ map pretty _fparams)
  pretty CExprVar    { .. } = pretty _var
  pretty CExprStr    { .. } = "string(\"" <> pretty _str <> "\")"
  pretty CExprNat    { .. } = "(nat)" <> pretty _nat
  pretty CExprBool   { .. } = pretty . T.toLower . T.pack . show $ _bool
  pretty CExprOption { _otype = CTUndef, .. } = case _val of
                                (Just a)  -> "some(" <> pretty a <> ")"
                                (Nothing) -> error $ "type inference failed for none()"
  pretty CExprOption { .. } = case _val of
                                (Just a)  -> "some<" <> pretty _otype <> ">(" <> pretty a <> ")"
                                (Nothing) -> "none<" <> pretty _otype <> ">()"
  pretty CExprList   { .. } = "list<" <> pretty _etype  <> ">{" <> commatize (map pretty _elems) <> "}"
  pretty CExprTuple  { .. } = "mktuple" <> (parens . commatize $ map pretty _items)
  pretty s@CExprSeq  { .. } = vcat (map (\x -> pretty x <> ";") (init . seqToList $ s))
                            <> line
                            <> "return" <+> pretty (last . seqToList $ s) <> ";"
  pretty CExprStmt   { _sd = CDef { _nm = "_", .. }, .. } = pretty _sbody
  pretty CExprStmt   { _sd = CDef { .. }, .. } = pretty _ty <+> pretty _nm <+> "=" <+> pretty _sbody

-- Generate lenses
makeLenses ''CDef
makeLenses ''CExpr
makeLenses ''CType

