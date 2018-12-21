{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module CIR.Expr where
import GHC.Generics
import Data.Aeson
import Data.MonoTraversable
import Common.Pretty
import Types.Context
import Data.Text.Prettyprint.Doc
import Data.Text (Text)
import Data.Map (Map)
import qualified Data.Text as T
import qualified Data.Map  as M
import Debug.Trace

-- Foldable is not possible as CExpr cannot be empty, FunctorM will do
class MonoFunctorM mono where
    omapM :: Monad m => (Element mono -> m (Element mono)) -> mono -> m mono

-- This class is for instances with types
class Typeful a where
    -- Get all libraries needed by a
    getincludes  :: a -> [Text]
    -- Unify with a type (inference) with a Type context
    unify        :: Context CType -> CType -> a -> a
    -- Return the type
    gettype      :: a -> CType
    -- Add types to context
    addctx       :: Context CType -> a -> Context CType
    -- Get number of free types
    getMaxVaridx :: a -> Int

-- This class is for instances with names
class Nameful a where
    getname      :: a -> Text

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
          | CExprCall   { _cd :: CDef, _cparams :: [CExpr] }
          -- Continuations
          | CExprSeq    { _left :: CExpr, _right :: CExpr }
          -- C++ statament
          | CExprStmt   { _sd :: CDef, _sbody :: CExpr }
          -- Reduced forms
          | CExprVar    { _var :: Text }
          | CExprStr    { _str :: Text }
          | CExprNat    { _nat :: Int }
          | CExprBool   { _bool :: Bool }
          | CExprTuple  { _items :: [CExpr] }
    deriving (Show, Eq, Generic, ToJSON)

instance Semigroup CExpr where
    (<>) = CExprSeq

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
    omap f   CExprCall   { .. } = CExprCall _cd $ fmap f _cparams
    omap f   CExprStmt   { .. } = CExprStmt _sd $ f _sbody
    omap f   CExprSeq    { .. } = CExprSeq (f _left) (f _right)
    omap f   CExprTuple  { .. } = CExprTuple $ fmap f _items
    omap f   CExprLambda { .. } = CExprLambda _lds $ f _lbody
    -- If it doesn't match anything, then it's a normal form, ignore
    omap _   other              = other

instance MonoFunctorM CExpr where
    omapM f   CExprCall   { .. } = mapM f _cparams >>= \ps -> return $ CExprCall _cd ps
    omapM f   CExprStmt   { .. } = f _sbody >>= \b -> return $ CExprStmt _sd b
    omapM f   CExprLambda { .. } = f _lbody >>= \b -> return $ CExprLambda _lds b
    omapM f   CExprSeq    { .. } = do { l <- f _left; r <- f _right; return $ CExprSeq l r }
    omapM f   CExprTuple  { .. } = mapM f _items >>= \items -> return $ CExprTuple items
    -- If it doesn't match anything, then it's a normal form, ignore
    omapM _   other              = return other

instance Typeful CDef where
    getincludes CDef  { .. } = getincludes _ty
    gettype CDef      { .. } = _ty
    unify ctx t CDef  { .. } = CDef _nm $ unify ctx t _ty
    addctx ctx CDef   { .. } = mergeCtx ctx $ M.singleton _nm _ty
    getMaxVaridx  = getMaxVaridx . gettype

instance Typeful CExpr where
    getincludes CExprSeq    { .. } = "proc":(getincludes _left ++ getincludes _right)
    getincludes CExprCall   { _cd = CDef { _nm = "show"}, .. } = "show" : concatMap getincludes _cparams
    getincludes CExprCall   { _cd = CDef { _nm = "gmatch"}, .. } = "variant" : concatMap getincludes _cparams
    getincludes CExprCall   { _cd = CDef { .. }, .. } = _nm : concatMap getincludes _cparams
    getincludes CExprStr    { .. } = ["String"]
    getincludes CExprNat    { .. } = ["nat"]
    getincludes CExprTuple  { .. } = "tuple" : concatMap getincludes _items
    getincludes CExprStmt   { .. } = "proc" : getincludes _sd ++ getincludes _sbody
    getincludes CExprLambda { .. } = concatMap getincludes _lds ++ getincludes _lbody
    getincludes CExprBool   { .. } = ["bool"]
    getincludes CExprVar    { .. } = []

    gettype s@CExprSeq { .. } = gettype . last . seqToList $ s
    gettype CExprCall { .. } = gettype _cd
    gettype CExprStr { .. } = CTBase "string"
    gettype CExprNat { .. } = CTBase "nat"
    gettype CExprTuple { .. } = CTExpr "tuple" $ map gettype _items
    gettype CExprStmt  { .. } = gettype _sd
    gettype CExprLambda { .. } = gettype _lbody
    gettype CExprBool { .. } = CTBase "bool"
    gettype _ = CTAuto

    unify ctx t CExprCall { _cd = CDef { .. },  .. }
        -- Return preserves the type
        | _nm == "return"    = CExprCall newD $ map (unify ctx t) _cparams
        -- A match preserves the type if the lambdas return it (omit matched object)
        | _nm  == "match"    = CExprCall newD $ head _cparams:map (unify ctx t) (tail _cparams)
        -- Match with something from the context
        | _nm `M.member` ctx =
            case ctx M.! _nm of
              (CTFunc { .. }) -> CExprCall newD $ zipWith (unify ctx) _fins _cparams
              (t) -> error $ "Cannot unify " ++ show _nm ++ " with " ++ show t
        -- Function call obfuscate the return type, ignore them
        | otherwise             = CExprCall newD _cparams
        where newD = CDef _nm $ unify ctx t _ty
    -- Or explicit if it comes from the first rule handling return calls
    unify ctx t s@CExprSeq { .. } = listToSeq first <> retexpr
        where retexpr = unify ctx t . last . seqToList $ s
              first   = init . seqToList $ s
    unify ctx t o = omap (unify ctx t) o

    -- Cowardly refuse to add expression to global context
    addctx ctx _ = ctx

    -- Get max varidx by getting the type first
    getMaxVaridx = getMaxVaridx . gettype

instance Typeful CType where
    getincludes CTFunc { .. } = getincludes _fret ++ concatMap getincludes _fins
    getincludes CTExpr { .. } = T.toLower _tbase : concatMap getincludes _tins
    getincludes CTVar  { .. } = concatMap getincludes _vargs
    getincludes CTBase { .. } = [T.toLower _base]
    getincludes _             = []

    -- Unify the same type
    unify _ a b | a == b = b
    -- Any type is better than undefined type
    unify _ t CTUndef {} = t
    unify _ CTUndef {} t = t
    -- Type is better than auto-type
    unify _ t CTAuto = t
    unify _ CTAuto _ = CTAuto
    -- Function types
    unify c CTFunc { _fret = a, _fins = ina} CTFunc { _fret = b, _fins = inb}
        | length ina == length inb = CTFunc (unify c a b) $ zipWith (unify c) ina inb
        | otherwise = error $ "Attempting to unify func types with different args" ++ show ina ++ " " ++ show inb
    -- Ignore Proc monad wrapped types
    unify c CTExpr { _tbase = "proc" , _tins = [a] } t = unify c a t
    unify c t CTExpr { _tbase = "proc" , _tins = [a] } = unify c t a
    -- Unify composite type expressions
    unify c CTExpr { _tbase = a , _tins = ina } CTExpr { _tbase = b , _tins = inb }
        | a == b && length ina == length inb = CTExpr a $ zipWith (unify c) ina inb
        | otherwise = error $ "Attempting to unify list types with different args" ++ show ina ++ " " ++ show inb
    -- Unify free parameters, here we're assuming Coq has already type-checked this
    unify _ CTFree { .. } t = t
    unify _ t CTFree { .. } = t
    -- Pointers go down, not up
    unify c CTPtr { .. } t = CTPtr $ unify c _inner t
    unify _ a b = error $ "Unsure how to unify " ++ show a ++ " " ++ show b

    -- Return the type itself
    gettype x = x

    -- Return number of free variables
    getMaxVaridx t = foldl max 0 $ getVaridxs t
        where getVaridxs CTFree { .. } = [_idx]
              getVaridxs CTFunc { .. } = getVaridxs _fret ++ concatMap getVaridxs _fins
              getVaridxs CTExpr { .. } = concatMap getVaridxs _tins
              getVaridxs CTPtr  { .. } = getVaridxs _inner
              getVaridxs _ = [0]

instance Nameful CDef where
    getname CDef { .. } = _nm

instance Nameful CExpr where
    getname CExprCall   { .. } = _nm _cd
    getname CExprStr    { .. } = _str
    getname CExprTuple  { .. } = T.pack . show $ _items
    getname CExprNat    { .. } = T.pack . show $ _nat
    getname CExprStmt   { .. } = _nm _sd
    getname CExprVar    { .. } = _var
    getname CExprBool   { .. } = T.pack . show $ _bool
    getname CExprLambda { .. } = "(lambda)"
    getname e = "(expr)"

-- Utility functions
-- Convert sequence to list of expressions
seqToList :: CExpr -> [CExpr]
seqToList CExprSeq { .. } = _left:seqToList _right
seqToList other           = [other]

-- Convert a list of expressions to a sequence
listToSeq :: [CExpr] -> CExpr
listToSeq []     = error "Empty sequence list given, unable to convert to expression"
listToSeq [a]    = a
listToSeq (a:ts) = a <> listToSeq ts

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
  pretty CTUndef {}     = error "Undef type found, type inference error"
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
  pretty CExprCall   { _cd = CDef { _nm = "return" }, _cparams = [a] } = "return" <+> pretty a <> ";"
  pretty CExprCall   { _cd = CDef { _nm = "eqb" }, _cparams = [a, b] } = pretty a <+> "==" <+> pretty b
  pretty CExprCall   { _cd = CDef { _nm = "ltb" }, _cparams = [a, b] } = pretty a <+> "<"  <+> pretty b
  pretty CExprCall   { _cd = CDef { _nm = "leb" }, _cparams = [a, b] } = pretty a <+> "<=" <+> pretty b
  pretty CExprCall   { _cd = CDef { _nm = "match" }, .. } = "match" <> (parens . breakcommatize $ _cparams)
  pretty CExprCall   { _cd = CDef { _ty = CTAuto, .. }, .. } = pretty _nm <> (parens . commatize $ map pretty _cparams)
  pretty CExprCall   { _cd = CDef { .. }, .. } =
    pretty _nm <> "<" <> pretty _ty <> ">" <> (parens . commatize $ map pretty _cparams)
  pretty CExprVar    { .. } = pretty _var
  pretty CExprStr    { .. } = "string(\"" <> pretty _str <> "\")"
  pretty CExprNat    { .. } = "(nat)" <> pretty _nat
  pretty CExprBool   { .. } = pretty . T.toLower . T.pack . show $ _bool
  pretty CExprTuple  { .. } = "mktuple" <> (parens . commatize $ map pretty _items)
  pretty s@CExprSeq  { .. } = vcat (map (\x -> pretty x <> ";") (init . seqToList $ s))
                            <> line
                            <> "return" <+> pretty (last . seqToList $ s) <> ";"
  pretty CExprStmt   { _sd = CDef { _nm = "_", .. }, .. } = pretty _sbody
  pretty CExprStmt   { _sd = CDef { .. }, .. } = pretty _ty <+> pretty _nm <+> "=" <+> pretty _sbody

