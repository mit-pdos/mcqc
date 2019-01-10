module Types.Classes where
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


