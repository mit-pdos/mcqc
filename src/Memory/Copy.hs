{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards  #-}
{-# LANGUAGE FlexibleContexts  #-}
module Memory.Copy (copyannotate, copyopt) where
import Common.Config (mutables)
import CIR.Expr
import Control.Monad.State
import Data.MonoTraversable (omap)
import Data.Text (Text)
import Debug.Trace

-- Count occurences of a specific binder, increases state
copyAnalysis :: Text -> CExpr -> State Int ()
copyAnalysis name CExprCall   { _fparams = v@CExprVar {..}:ps, .. }
    -- If the function mutates, increase references by 1 always
    | (_fname `elem` mutables) && (name == _var) = get >>= \n -> put (n+1) >>= continue nextarg
    | otherwise                                  = continue v () >>= continue nextarg
    where continue = \e _ -> copyAnalysis name e
          nextarg  = CExprCall _fname ps
-- Recurse in two directions, parallel to the next argument and down, into the argument h
copyAnalysis name CExprCall   { _fparams = h:ps, .. } = do { copyAnalysis name h; copyAnalysis name nextarg }
    where nextarg  = CExprCall _fname ps
copyAnalysis name CExprLambda { .. }
    -- Shadowing can happen in a lambda arg, stop
    | name `elem` _largs = return ()
    | otherwise          = copyAnalysis name _lbody
copyAnalysis name CExprSeq    { .. } = do { copyAnalysis name _left; copyAnalysis name _right }
copyAnalysis name CExprStmt   { .. }
    -- Shadowing can happen in a statement, stop
    | name == _sname = return ()
    | otherwise      = copyAnalysis name _sbody
copyAnalysis name CExprTuple  { .. } = mapM_ (copyAnalysis name) _items
-- If the function does not mutate, increase references only if a mutating reference has been seen already
copyAnalysis name CExprVar    { .. }
    | name == _var = get >>= \n -> if n > 0 then put (n+1) else return ()
    | otherwise    = return ()
-- Call with no parameters, stop
copyAnalysis _ CExprCall      { .. } = return ()
copyAnalysis _ _ = return ()

-- Annotate all occurences of a binder, decreases state
copyAnnotate:: Text -> CExpr -> State Int CExpr
copyAnnotate name CExprCall { .. }
    | _fname `elem` mutables = (mapM replaceVar _fparams) >>= \m -> return $ CExprCall _fname m
    | otherwise              = (mapM (copyAnnotate name) _fparams) >>= \m -> return $ CExprCall _fname m
    where replaceVar v@CExprVar { .. }
              | name == _var = do {
                   m <- get;
                   put (m-1);
                   if m > 1
                     then copyAnnotate name v >>= \m -> return $ CExprCall "copy" [m]
                     else return v
                   }
              | otherwise = copyAnnotate name v
          replaceVar e = copyAnnotate name e
copyAnnotate name e = omapM (copyAnnotate name) e

-- Top level, to annotate, pass a list of binders and an expression to be annotated with copy()
copyannotate :: [Text] -> CExpr -> CExpr
copyannotate (name:ns) e = copyannotate ns $ evalState runstate 0
    where runstate = copyAnalysis name e >>= (\_ -> copyAnnotate name e)
copyannotate []        e = e

-- Top level copy pass
copyopt :: CExpr -> CExpr
copyopt CExprLambda { .. } = CExprLambda _largs $ copyannotate _largs _lbody
copyopt o = omap copyopt o
