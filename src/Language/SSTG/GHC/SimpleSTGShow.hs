{-# LANGUAGE StandaloneDeriving #-}
module Vis.GHC.SimpleSTGShow where

import Vis.GHC.SimpleSTG
import StgSyn
import CoreSyn (AltCon(..))
import DataCon
import Name
import Literal
import Type

import PrimOp
import ForeignCall

import GHC.Show

deriving instance Show PrimCall
deriving instance Show ForeignCall
deriving instance Show CCallSpec
deriving instance Show CCallTarget
deriving instance Show CCallConv
deriving instance (Show id) => Show (SStgExpr id)
deriving instance (Show id) => Show (SStgArg id)
deriving instance (Show id) => Show (SStgAlt id)
deriving instance (Show id) => Show (SStgBinding id)
deriving instance (Show id) => Show (SStgRhs id)
deriving instance (Show id) => Show (SStgAltCon id)
deriving instance Show SUpdateFlag

instance (Show StgOp) where
  showsPrec n (StgPrimOp op) = showParen (n > appPrec) $ showString $ 
                                 unwords ["StgPrimOp", show op]
  showsPrec n (StgPrimCallOp pcall) = showParen (n > appPrec) $ showString $ 
                                        unwords ["StgPrimCallOp", show pcall]
  showsPrec n (StgFCallOp fcall _) = showParen (n > appPrec) $ showString $ 
                                       unwords ["StgFCallOp", show fcall]
