{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
module Types.Context where
import System.IO.Unsafe
import Data.Map
import Data.Text (Text)
import Control.Monad.State
import Debug.Trace

-- Named context
type Context a = Map Text a

-- Print key values correctly
printCtx :: Show a => Context a -> IO ()
printCtx = putStr . concatMap (++"\n") . elems . mapWithKey (\k v -> show k ++ " : " ++ show v)

-- Print Ctx unsafe
unsafePrintCtx :: Show a => Bool -> Context a -> Bool
unsafePrintCtx b = const b . unsafePerformIO . printCtx

-- Merge two contexts, always prefer old definitions
mergeCtx :: Show a => Context a -> Context a -> Context a
mergeCtx = unionWithKey (\k va vb -> trace ("Warning: Polyrmorphism in context for " ++ show k ++ " keeping this declaration " ++ show vb) vb)

