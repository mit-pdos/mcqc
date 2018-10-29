{-# LANGUAGE DeriveAnyClass, DeriveGeneric, DuplicateRecordFields, TemplateHaskell  #-}
module CIR.Decl where
import GHC.Generics
import Control.Lens
import CIR.Expr
import Data.Aeson
import Data.Text (Text)
import Data.Aeson.Encode.Pretty
import qualified Data.ByteString.Lazy.Char8 as B

-- Global scope C definitions,
-- TODO: Records, Types, global vars
data CDecl =
    CDFunc    { _fn :: Text, _ftype :: CType, _fargs :: [Text], _fbody :: CExpr }
    | CDType  { _tname :: Text, _tval :: CType }
    | CDExpr  { _ename :: Text, _expr :: CExpr }
    | CDInd   { _iname :: Text, _itype ::CType, _ictors :: [(Text, [CType])] }
    | CDEmpty {}
  deriving (Eq, Generic, ToJSON)

-- Generate Lens code
makeLenses ''CDecl

instance Show CDecl where
    show = B.unpack . encodePretty

