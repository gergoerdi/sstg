module SSTG.Serialization (writeStgb, readStgb) where

import SSTG.SimpleSTG

import HscTypes
import Module
import Binary
import Name
import UniqFM
import FastMutInt
import Unique
import UniqSupply
import IfaceEnv
import SrcLoc (noSrcSpan)
import TcRnMonad

import Data.Array
import Data.List
import Data.IORef
import Control.Monad
import Data.Word
import Control.Arrow

-- Based on GHC's BinIface
writeStgb :: FilePath -> [SStgBinding Name] -> IO ()
writeStgb fn sstgs = do
  bh <- openBinMem initBinMemSize       
       
  -- Remember where the dictionary pointer will go
  fsDict_p_p <- tellBin bh
  put_ bh fsDict_p_p	-- Placeholder for ptr to dictionary
      
  -- Remember where the symbol table pointer will go
  nameDict_p_p <- tellBin bh
  put_ bh nameDict_p_p

  nameDict <- mkFastDict
  fsDict <- mkFastDict            
  ud <- newWriteState (putSymbol nameDict) (putSymbol fsDict)
  bh <- return $ setUserData bh ud
            
  put_ bh sstgs
      
  -- Write the symtab pointer at the front of the file
  nameDict_p <- tellBin bh	        -- This is where the symtab will start
  putAt bh nameDict_p_p nameDict_p	-- Fill in the placeholder
  seekBin bh nameDict_p		-- Seek back to the end of the file      
      
  -- Write the symbol table itself
  putSymbolTable bh nameDict

  -- NB. write the dictionary after the symbol table, because
  -- writing the symbol table may create more dictionary entries.

  -- Write the dictionary pointer at the fornt of the file
  fsDict_p <- tellBin bh	        -- This is where the dictionary will start
  putAt bh fsDict_p_p fsDict_p	-- Fill in the placeholder
  seekBin bh fsDict_p		-- Seek back to the end of the file

  -- Write the dictionary itself
  fsDict_next <- readFastMutInt $ fsd_next fsDict
  fsDict_map <- readIORef $ fsd_map fsDict
  putDictionary bh fsDict_next fsDict_map    
  
  writeBinMem bh fn
  
readStgb :: GhcMonad m => FilePath -> m [SStgBinding Name]
readStgb fn = do
  env <- mkEnv
  liftIO . runIOEnv env $ do
    update_nc <- mkNameCacheUpdater
    liftIO $ do
      bh <- readBinMem fn
  
      dict_p <- get bh
      data_p <- tellBin bh          -- Remember where we are now           
      seekBin bh dict_p  
      dict <- getDictionary bh
      seekBin bh data_p             -- Back to where we were before
        
      ud <- newReadState dict
      bh <- return (setUserData bh ud)        
             
      symtab_p <- get bh     -- Get the symtab ptr
      data_p <- tellBin bh   -- Remember where we are now
      seekBin bh symtab_p
      symtab <- getSymbolTable bh update_nc
      seekBin bh data_p             -- Back to where we were before
      let ud = getUserData bh
      bh <- return $! setUserData bh ud{ud_symtab = symtab}
      get bh

  where mkEnv = do 
          session <- getSession
          us <- liftIO $ newIORef =<< mkSplitUniqSupply '_'    
          return $ Env { env_top = session,
                         env_us = us,
                         env_gbl = (),
                         env_lcl = () }


initBinMemSize :: Int
initBinMemSize = 1024 * 1024

data FastDict a = FastDict { fsd_next :: !FastMutInt,
                             fsd_map :: !(IORef (UniqFM (Int, a))) }
                      
mkFastDict :: IO (FastDict a)
mkFastDict = do
  next <- newFastMutInt
  writeFastMutInt next 0
  map <- newIORef emptyUFM
  return $ FastDict { fsd_next = next, fsd_map = map }

putSymbol :: (Uniquable a) => FastDict a -> BinHandle -> a -> IO ()
putSymbol FastDict{ fsd_next = fsd_next, fsd_map = fsd_map } bh sym = do
  map <- readIORef fsd_map
  case lookupUFM map sym of
    Just (idx, _) -> put_ bh (fromIntegral idx :: Word32)
    Nothing -> do
      idx <- readFastMutInt fsd_next
      put_ bh (fromIntegral idx :: Word32)
      writeFastMutInt fsd_next (succ idx)
      writeIORef fsd_map $! addToUFM map sym (idx, sym)
            
putSymbolTable :: BinHandle -> FastDict Name -> IO ()
putSymbolTable bh FastDict{ fsd_next = fsd_next, fsd_map = fsd_map} = do
  idx <- readFastMutInt fsd_next
  put_ bh idx
  map <- readIORef fsd_map
  let names = elems (array (0, idx - 1) (eltsUFM map))
  forM_ names $ \name -> do
    let mmod = nameModule_maybe name        
    put_ bh ((fmap (modulePackageId &&& moduleName) mmod, nameOccName name) :: OnDiskName)

getSymbolTable :: BinHandle -> NameCacheUpdater (Array Int Name) -> IO (Array Int Name)
getSymbolTable bh update_namecache = do
  sz <- get bh
  od_names <- sequence (replicate sz (get bh))
  update_namecache $ \namecache -> 
    let (namecache', names) = mapAccumR fromOnDiskName namecache od_names
        arr = listArray (0, sz-1) names 
    in (namecache', arr)

type OnDiskName = (Maybe (PackageId, ModuleName), OccName)

fromOnDiskName :: NameCache -> OnDiskName -> (NameCache, Name)
fromOnDiskName nc (Nothing, occ) = (nc', name)
  where us = nsUniqs nc
        (us', _) = splitUniqSupply us
        uniq = uniqFromSupply us
        name = mkInternalName uniq occ noSrcSpan        
        nc' = nc{ nsUniqs = us' }
  
fromOnDiskName nc (Just modinfo, occ) = 
  case lookupOrigNameCache cache mod occ of
     Just name -> (nc, name)
     Nothing   -> let us = nsUniqs nc
                      uniq = uniqFromSupply us
                      name = mkExternalName uniq mod occ noSrcSpan
                      cache' = extendNameCache cache mod occ name
                      (us', _)  = splitUniqSupply us
                      nc' = nc{ nsUniqs = us', nsNames = cache' }
                  in (nc', name)
  
    where mod = uncurry mkModule modinfo
          cache = nsNames nc          
  
  --   where mod = fmap (uncurry mkModule) modinfo
  --         cache = nsNames nc          
  --         mkName = case mod of
  --           Just mod -> mkExternalName `flip` mod
  --           Nothing -> mkInternalName
