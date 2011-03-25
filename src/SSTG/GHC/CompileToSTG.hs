module SSTG.GHC.CompileToSTG (compileToStg, Stg, CompileFlags(..), CompileRes(..)) where

import GHC
import HscTypes
import HscMain
import GHC.Paths (libdir)
import CorePrep (corePrepPgm)
import TyCon (isDataTyCon)
import CoreToStg (coreToStg)
import SimplStg (stg2stg)
import StgSyn (GenStgBinding)
import CmdLineParser

import Control.Monad
import Data.Maybe
import Control.Monad.Writer (runWriter, tell)

parseAndTypecheck :: (GhcMonad m) => ModSummary -> m DesugaredModule
parseAndTypecheck mod = parseModule mod >>= typecheckModule >>= desugarModule

type Stg id = [(GenStgBinding id id, [(id, [id])])]

compileModule :: (GhcMonad m) => ModSummary -> m (Stg Id)
compileModule mod = do
  core <- parseAndTypecheck mod
  let guts = coreModule core
  guts' <- hscSimplify guts
  (_iface, _changed, _details, cgguts) <- hscNormalIface guts' Nothing
  
  liftIO $ do
    let dflags = ms_hspp_opts mod
        coreBinds = cg_binds cgguts
        tycons = cg_tycons cgguts
        data_tycons = filter isDataTyCon tycons      
        this_mod = cg_module cgguts    
    prepd_binds <- corePrepPgm dflags coreBinds data_tycons
    stg_binds <- coreToStg (modulePackageId this_mod) prepd_binds    
    (stg_binds', _ccs) <- stg2stg dflags this_mod stg_binds
    return stg_binds'
  
data CompileFlags = DumpStg
                  deriving (Eq, Show)
  
data CompileRes = CompileRes { cr_packageId :: PackageId,
                               cr_outDir :: Maybe FilePath,
                               cr_opts :: [CompileFlags],
                               cr_stgs :: [(ModSummary, Stg Id)] }
  
compileToStg :: [String] -> IO CompileRes
compileToStg args = runGhc (Just libdir) $ do
  dflags <- getSessionDynFlags  
  (dflags, largs, _) <- parseDynamicFlags dflags (map noLoc args)
  _ <- setSessionDynFlags dflags
  
  let flags = [Flag "dump-stg" (NoArg (tell [DumpStg])) Supported]
  let ((lfilenames, _, _), opts) = runWriter $ processArgs flags largs
      filenames = map unLoc lfilenames

  targets <- mapM (guessTarget `flip` Nothing) filenames
  setTargets targets
  _ <- load LoadAllTargets
  mods <- depanal [] False
  result <- forM mods $ \mod -> do
    let name = moduleNameString . moduleName . ms_mod $ mod
    case ms_hsc_src mod of 
      HsBootFile -> do
        liftIO $ putStrLn . unwords $ ["Skipping", name] 
        return Nothing
      _ -> do 
        liftIO $ putStrLn . unwords $ ["Compiling", name]
        stg <- compileModule mod
        return $ Just (mod, stg)
        
  return $ CompileRes (thisPackage dflags) (hiDir dflags) opts (catMaybes result)
