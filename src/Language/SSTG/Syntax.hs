module Language.SSTG.Syntax (
  -- * Simplified STG
  SStgBindingGroup,
  SStgBinding(..), SStgRhs(..), 
  SStgExpr(..), SStgArg(..), SStgAlt(..), SStgPat(..),
  -- * Conversion from GHC's representation
  simplifyBinding,
  module Literal,
  UpdateFlag(..)
  ) where

import StgSyn
import CoreSyn (AltCon(..))
import Name
import Literal
import Binary
import PrimOp
import Unique

import Control.Arrow
import Control.Applicative                                       
import Data.IntMap (IntMap, (!))
import qualified Data.IntMap as IntMap

getTag :: BinHandle -> IO Char
getTag = get

newtype StgOp_Binary = StgOp_Binary { getStgOp :: StgOp }

stgPrimOp_tag = 'o'
stgPrimCallOp_tag = 'c'
stgFCallOp_tag = 'f'

stgPrimOp_map :: IntMap PrimOp
stgPrimOp_map = IntMap.fromList $ map (\ op -> (primOpTag op, op)) allThePrimOps

instance Binary StgOp_Binary where
  put_ bh op = case getStgOp op of
    (StgPrimOp op) -> put_ bh stgPrimOp_tag >> put_ bh (primOpTag op)
    (StgPrimCallOp (PrimCall clabel pkgId)) -> put_ bh stgPrimCallOp_tag >> put_ bh clabel >> put_ bh pkgId
    (StgFCallOp fcall u) -> put_ bh stgFCallOp_tag >> put_ bh fcall >> put_ bh (getKey u)
    
  get bh = StgOp_Binary <$> do
    tag <- getTag bh
    case () of
      _ | tag == stgPrimOp_tag -> StgPrimOp <$> (stgPrimOp_map!) <$> get bh
        | tag == stgPrimCallOp_tag -> StgPrimCallOp <$> (PrimCall <$> get bh <*> get bh)
        | tag == stgFCallOp_tag -> StgFCallOp <$> get bh <*> (mkUniqueGrimily <$> get bh)
                   
-- |Simplified version of 'GenStgExpr'.
data SStgExpr id = SStgApp id [SStgArg id]
                 | SStgLit Literal
                 | SStgConApp id [SStgArg id]
                 | SStgOpApp StgOp [SStgArg id]
                 | SStgLam [id] (SStgExpr id)
                 | SStgCase id (SStgExpr id) [SStgAlt id]
                 | SStgLet (SStgBindingGroup id) (SStgExpr id)
                   
sStgApp_tag = 'a'                   
sStgLit_tag = 'l'
sStgConApp_tag = 'c'
sStgOpApp_tag = 'o'
sStgLam_tag = '\\'
sStgCase_tag = 's' -- switch
sStgLet_tag = 'b' -- bind

instance (Binary id) => Binary (SStgExpr id) where
  put_ bh (SStgApp f args) = put_ bh sStgApp_tag >> put_ bh f >> put_ bh args
  put_ bh (SStgLit lit) = put_ bh sStgLit_tag >> put_ bh lit
  put_ bh (SStgConApp c args) = put_ bh sStgConApp_tag >> put_ bh c >> put_ bh args
  put_ bh (SStgOpApp op args) = put_ bh sStgOpApp_tag >> put_ bh (StgOp_Binary op) >> put_ bh args
  put_ bh (SStgLam vars body) = put_ bh sStgLam_tag >> put_ bh vars >> put_ bh body
  put_ bh (SStgCase var expr alts) = put_ bh sStgCase_tag >> put_ bh var >> put_ bh expr >> put_ bh alts
  put_ bh (SStgLet binds body) = put_ bh sStgLet_tag >> put_ bh binds >> put_ bh body

  get bh = do
    tag <- getTag bh
    case () of
      _ | tag == sStgApp_tag -> SStgApp <$> get bh <*> get bh
        | tag == sStgLit_tag -> SStgLit <$> get bh
        | tag == sStgConApp_tag -> SStgConApp <$> get bh <*> get bh
        | tag == sStgOpApp_tag -> SStgOpApp <$> (getStgOp <$> get bh) <*> get bh
        | tag == sStgLam_tag -> SStgLam <$> get bh <*> get bh
        | tag == sStgCase_tag -> SStgCase <$> get bh <*> get bh <*> get bh
        | tag == sStgLet_tag -> SStgLet <$> get bh <*> get bh
    
-- |Simplified version of 'GenStgArg'.
data SStgArg id = SStgArgVar id
                | SStgArgLit Literal
                --  | SStgArgType Type -- TODO: What the hell is a Type argument?

sStgArgVar_tag = 'v'
sStgArgLit_tag = 'l'

instance (Binary id) => Binary (SStgArg id) where
  put_ bh (SStgArgVar x) = put_ bh sStgArgVar_tag >> put_ bh x
  put_ bh (SStgArgLit lit) = put_ bh sStgArgLit_tag >> put_ bh lit
  
  get bh = do
    tag <- getTag bh
    case () of
      _ | tag == sStgArgVar_tag -> SStgArgVar <$> get bh
        | tag == sStgArgLit_tag -> SStgArgLit <$> get bh

-- |Simplified version of 'GenStgAlt'
data SStgAlt id = SStgAlt (SStgPat id) (SStgExpr id)

instance (Binary id) => Binary (SStgAlt id) where
  put_ bh (SStgAlt pat expr) = put_ bh pat >> put_ bh expr 
  get bh = SStgAlt <$> get bh <*> get bh
                          
-- |Simplified version of `AltCon`. 
data SStgPat id = SStgPatData id [id] -- ^ The list of variables bound by the constructor is merged from `GenStgAlt`.
                | SStgPatLit Literal
                | SStgPatWildcard
                     
sStgAltData_tag = 'd'
sStgAltLit_tag = 'l'                     
sStgAltWildcard_tag = 'w'
                     
instance (Binary id) => Binary (SStgPat id) where
  put_ bh (SStgPatData con vars) = put_ bh sStgAltData_tag >> put_ bh con >> put_ bh vars  
  put_ bh (SStgPatLit lit) = put_ bh sStgAltLit_tag >> put_ bh lit
  put_ bh (SStgPatWildcard) = put_ bh sStgAltWildcard_tag
  
  get bh = do
    tag <- getTag bh
    case () of
      _ | tag == sStgAltData_tag -> SStgPatData <$> get bh <*> get bh
        | tag == sStgAltLit_tag -> SStgPatLit <$> get bh
        | tag == sStgAltWildcard_tag -> return SStgPatWildcard    
  
-- |Simplified version of 'GenStgBinding'.
data SStgBinding id = SStgBinding id (SStgRhs id)

type SStgBindingGroup id = [SStgBinding id]

instance (Binary id) => Binary (SStgBinding id) where
  put_ bh (SStgBinding name rhs) = put_ bh name >> put_ bh rhs
  get bh = SStgBinding <$> get bh <*> get bh
                              
-- |Simplified version of 'GenStgRhs'.
data SStgRhs id = SStgRhsCon id [SStgArg id]
                | SStgRhsClosure UpdateFlag [id] (SStgExpr id)
                  
sStgRhsCon_tag = 'c'
sStgRhsClosure_tag = '\\'

instance (Binary id) => Binary (SStgRhs id) where
  put_ bh (SStgRhsCon con args) = put_ bh sStgRhsCon_tag >> put_ bh con >> put_ bh args
  put_ bh (SStgRhsClosure update args body) = put_ bh sStgRhsClosure_tag >> put_ bh (UpdateFlag_Binary update) >> put_ bh args >> put_ bh body
  
  get bh = do
    tag <- getTag bh
    case () of
      _ | tag == sStgRhsCon_tag -> SStgRhsCon <$> get bh <*> get bh
        | tag == sStgRhsClosure_tag -> SStgRhsClosure <$> (getUpdateFlag <$> get bh) <*> get bh <*> get bh
                          
newtype UpdateFlag_Binary = UpdateFlag_Binary { getUpdateFlag :: UpdateFlag }                         

updatable_tag = 'u'
reEntrant_tag = 'r'
singleEntry_tag = 's'

instance Binary UpdateFlag_Binary where
  put_ bh ufb = put_ bh $ case getUpdateFlag ufb of
    Updatable -> updatable_tag
    ReEntrant -> reEntrant_tag
    SingleEntry -> singleEntry_tag
  
  get bh = UpdateFlag_Binary <$> do
    tag <- getTag bh
    case () of
      _ | tag == updatable_tag -> return Updatable
        | tag == reEntrant_tag -> return ReEntrant
        | tag == singleEntry_tag -> return SingleEntry

simplifyBinding :: StgBinding  -> SStgBindingGroup Name
simplifyBinding (StgNonRec x body) = [SStgBinding (getName x) (simplifyRhs body)]
simplifyBinding (StgRec binds) = map (uncurry SStgBinding . (getName *** simplifyRhs)) binds

simplifyRhs :: StgRhs -> SStgRhs Name
simplifyRhs (StgRhsCon _ con args) = SStgRhsCon (getName con) $ map simplifyArg args
simplifyRhs (StgRhsClosure _ _ _ update _ args expr) = SStgRhsClosure update (map getName args) (simplifyExpr expr)

simplifyArg :: StgArg -> SStgArg Name
simplifyArg (StgVarArg x) = SStgArgVar (getName x)
simplifyArg (StgLitArg lit) = SStgArgLit lit
simplifyArg (StgTypeArg _) = error "StgTypeArg"

simplifyExpr :: StgExpr -> SStgExpr Name
simplifyExpr (StgApp f args) = SStgApp (getName f) $ map simplifyArg args
simplifyExpr (StgLit lit) = SStgLit lit
simplifyExpr (StgConApp con args) = SStgConApp (getName con) $ map simplifyArg args
simplifyExpr (StgOpApp op args _) = SStgOpApp op $ map simplifyArg args
simplifyExpr (StgLam _ xs body) = SStgLam (map getName xs) $ simplifyExpr body
simplifyExpr (StgCase e _ _ var _ _ alts) = SStgCase (getName var) (simplifyExpr e) (map simplifyAlt alts)
simplifyExpr (StgLet binding body) = SStgLet (simplifyBinding binding) $ simplifyExpr body
simplifyExpr (StgLetNoEscape _ _ binding body) = simplifyExpr (StgLet binding body)
simplifyExpr (StgSCC _ e) = simplifyExpr e
simplifyExpr (StgTick _ _ e) = simplifyExpr e

simplifyAlt (con, vars, _, e) = SStgAlt con' $ simplifyExpr e
  where con' = case con of
          DataAlt con -> SStgPatData (getName con) (map getName vars)
          LitAlt lit -> SStgPatLit lit
          DEFAULT -> SStgPatWildcard
