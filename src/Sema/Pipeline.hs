{-# LANGUAGE RecordWildCards, OverloadedStrings  #-}
module Sema.Pipeline where
import Sema.Nat

-- Update all semantic transforms here to create a pipeline
semantics = natSemantics
