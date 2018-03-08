module Boogie.Z3.Minimize where

import           Control.Applicative
import           Control.Lens
import           Control.Monad.IO.Class
import           Control.Monad

import           Data.Maybe
import qualified Data.Map as Map

import           Z3.Monad

import           Boogie.AST
import           Boogie.Solver
import           Boogie.Z3.Eval
import           Boogie.Z3.GenMonad
        
-- | 'minimizeModel' @model@ : Minimize all objective functions one by one starting from @model@               
minimizeModel :: Model -> Z3Gen Model
minimizeModel model = 
    do objs <- objectives
       foldM minimizeOne model objs

-- | 'minimizeOne' @model obj@ : Minimize objective function @obj@ starting from @model@
-- The algorithm basically does the following: 
-- 1. Check if the current objective value v can be lowered at all; if not, then stop
-- 2. If yes, check if it can be zero; if yes, then stop.
-- 3. otherwise do a binary search in [1, v - 1]  
minimizeOne :: Model -> AST -> Z3Gen Model       
minimizeOne model obj = do       
      v <- evalObj model 
      go model Nothing (v-1) (v+1)
    where
      go :: Model -> Maybe Integer -> Integer -> Integer -> Z3Gen Model
      go m loMb pivot hi
         | isNothing loMb || hi - fromJust loMb > 2 =
             do push
                assertPivot pivot
                (_, modelMb) <- getModel
                case modelMb of
                  Just m' ->
                      case loMb of
                        Nothing ->
                            do 
                               debug ("go SAT Nothing:" ++ show (loMb, pivot, hi))
                               go m' (Just (-1)) 0 (pivot + 1)
                        Just lo ->
                            do lv <- evalObj m'
                               let pivot' = lo + (lv + 1 - lo) `div` 2
                               debug ("go SAT Just:" ++ show (loMb, pivot, hi))
                               go m' loMb pivot' (lv + 1)
                  Nothing ->
                      do let pivot' = pivot + ((hi - pivot) `div` 2)
                         debug ("go UNSAT:" ++ show (loMb, pivot, hi))
                         pop 1
                         go m (Just pivot) pivot' hi
         | otherwise = do
            debug ("go DONE:" ++ show (loMb, pivot,hi))
            assertPivot pivot
            return m

      assertPivot pivot =
          do piv <- mkIntNum pivot
             pivCnstr <- mkLe obj piv
             assert pivCnstr

      evalObj :: Model -> Z3Gen Integer
      evalObj m =
          do Just objVal <- eval m obj
             v <- getInt objVal
             return v

-- | ASTs representing objective functions.
objectives :: Z3Gen [AST]
objectives =
    do intMbs <- mapM (uncurry intAst) =<< uses refMap Map.toList
       return $ catMaybes intMbs
    where
      intAst :: TaggedRef -> AST -> Z3Gen (Maybe AST)
      intAst (LogicRef t _ref) ast =
          case t of 
            IntType -> Just <$> absT ast
            IdType ident types ->
                do (_, _, proj) <- lookupCustomType ident types
                   val <- mkApp proj [ast]
                   Just <$> absT val
            _ -> return Nothing
      intAst _ _ = return Nothing
          
      absT :: AST -> Z3Gen AST
      absT ast =
          do zero <- mkIntNum 0
             less <- mkLt ast zero
             negAst <- mkUnaryMinus ast
             mkIte less negAst ast
