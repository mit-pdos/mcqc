{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
module CIR.Decl where
import CIR.Expr hiding (omapM)
import GHC.Generics
import Data.Aeson
import Data.MonoTraversable
import Data.Text (Text)
import Data.Aeson.Encode.Pretty
import qualified Data.ByteString.Lazy.Char8 as B

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
    -- XXX: Looks wrong
    omap _ d = d

-- Expression map
emap :: (CExpr -> CExpr) -> CDecl -> CDecl
emap f CDFunc { .. } = CDFunc _fd _fargs $ f _fbody
emap f CDSeq { .. } = emap f _left <> emap f _right
emap _ d = d

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

