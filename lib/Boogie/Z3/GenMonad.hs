{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveDataTypeable #-}
module Boogie.Z3.GenMonad
    ( Z3Gen
    , evalZ3Gen
    , evalZ3GenWith
    , TaggedRef(..)
    , sortMap
    , ctorMap
    , refMap
    , debug
    , debug1
    , lookup'
    , justElse
    , justElseM
    , lookupSort
    , lookupCustomType
    , lookupCustomCtor
    , lookupCustomProj
    , lookupTupleSort
    , lookupCtor
    , tupleSymbol
    , customSymbol
    , typeString
    ) where

import           Control.Applicative
import           Control.Lens ((%=), view, _1, _2, _3, uses, makeLenses)
import           Control.Monad.Trans.State
import           Control.Monad.Trans

import           Data.List (intercalate)
import           Data.Generics
import           Data.Maybe
import qualified Data.Map as Map
import           Data.Map (Map)

import           Z3.Monad

import           Boogie.AST
import           Boogie.PrettyAST ()

data TaggedRef = LogicRef Type Ref
               | MapRef Type Ref
                 deriving (Eq, Ord, Show, Data, Typeable)

data Custom = Custom Type Int
            deriving (Eq, Ord, Show)


data Z3State = Z3State
    { _ctorMap :: 
          Map [Type] 
                 (Sort, FuncDecl, [FuncDecl]) -- ^ Maps a list of types to a
                                              -- a tuple of them, and the
                                              -- associated constructor.
    , _sortMap :: Map Type Sort               -- ^ Maps types to sorts
    , _refMap  :: Map TaggedRef AST           -- ^ Maps references to their
                                              -- Z3 AST node.
    , _customMap :: 
        Map (Id,[Type])
                (Sort, FuncDecl, FuncDecl)    -- ^ Map from identifier and
                                              -- type arguments to a 
                                              -- an uninterpreted type

    }

makeLenses ''Z3State

instance MonadZ3 Z3Gen where
    getSolver = lift getSolver
    getContext = lift getContext

type Z3Gen = StateT Z3State Z3

emptyEnv :: Z3State
emptyEnv = Z3State Map.empty Map.empty Map.empty Map.empty

evalZ3Gen :: Z3Gen a -> IO a
evalZ3Gen act = evalZ3 $ evalStateT act emptyEnv

evalZ3GenWith :: Z3Env -> Z3Gen a -> IO a
evalZ3GenWith env act =
    evalZ3WithEnv (evalStateT act emptyEnv) env


debug :: MonadIO m => String -> m ()
debug = const (return ()) -- liftIO . putStrLn

debug1 :: MonadIO m => String -> m ()
debug1 = const (return ())

lookup' :: Ord k => String -> k -> Map k a -> a
lookup' errMsg key m =
  case Map.lookup key m of
    Just a -> a
    Nothing -> error errMsg

justElse :: Maybe a -> a -> a
justElse = flip fromMaybe

justElseM :: Monad m => Maybe a -> m a -> m a
justElseM mb v = maybe v return mb

lookupSort :: Type -> Z3Gen Sort
lookupSort ttype =
    do sortMb <- uses sortMap (Map.lookup ttype)
       justElseM sortMb $
         do s <- typeToSort ttype
            sortMap %= Map.insert ttype s
            return s
    where
      -- | Construct a type map.
      typeToSort :: Type -> Z3Gen Sort
      typeToSort t =
          case t of
            IntType  -> mkIntSort
            BoolType -> mkBoolSort
            MapType _ argTypes resType ->
                do tupleArgSort <- lookupTupleSort argTypes
                   resSort <- lookupSort resType
                   mkArraySort tupleArgSort resSort
            IdType ident types -> view _1 <$> lookupCustomType ident types

lookupCustomType :: Id -> [Type] -> Z3Gen (Sort, FuncDecl, FuncDecl)
lookupCustomType ident types =
    do custMb <- uses customMap (Map.lookup (ident, types))
       justElseM custMb $
         do let str = customSymbol ident types
            sym <- mkStringSymbol str
            projSym <- mkStringSymbol (str ++ "_proj")
            intSort <- lookupSort IntType
            (sort, ctor, [proj]) <- mkTupleSort sym [(projSym, intSort)]
            let res = (sort, ctor, proj)
            customMap %= Map.insert (ident, types) res
            return res

lookupCustomCtor :: Id -> [Type] -> Z3Gen FuncDecl
lookupCustomCtor ident types =
    view _2 <$> lookupCustomType ident types

lookupCustomProj :: Id -> [Type] -> Z3Gen FuncDecl
lookupCustomProj ident types =
    view _3 <$> lookupCustomType ident types

lookupTupleSort :: [Type] -> Z3Gen Sort
lookupTupleSort types = ( \ (a,_,_) -> a) <$> lookupCtor types

-- | Construct a tuple from the given arguments
lookupCtor :: [Type] -> Z3Gen (Sort, FuncDecl, [FuncDecl])
lookupCtor types =
    do sortMb <- uses ctorMap (Map.lookup types)
       justElseM sortMb $
         do sorts   <- mapM lookupSort types
            let tupStr = tupleSymbol types
            argSyms <- mapM (mkStringSymbol . (tupStr ++) . show) 
                             [1 .. length types]
            sym     <- mkStringSymbol tupStr
            tupRes  <- mkTupleSort sym (zip argSyms sorts)
            ctorMap %= Map.insert types tupRes
            return tupRes

-- | Type name for the symbol for the sort
tupleSymbol :: [Type] -> String
tupleSymbol ts = intercalate "_" (map typeString ts) ++ "SYMBOL"

-- | Type name for the symbol for the sort
customSymbol :: Id -> [Type] -> String
customSymbol ident ts = intercalate "_" (ident : map typeString ts) ++ "_CUSTOM"

-- | Symbol name for a type
typeString :: Type -> String
typeString t =
   case t of
     IntType -> "int"
     BoolType -> "bool"
     MapType _ args res -> 
         concat ["(", tupleSymbol args, ")->", typeString res]
     IdType ident types ->
         intercalate "_" ("IdType":ident:map typeString types)
