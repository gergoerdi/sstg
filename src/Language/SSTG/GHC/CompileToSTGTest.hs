module Main where

import SSTG.SimpleSTG
import SSTG.Serialization

import Outputable
import HscTypes

import Name
import Unique
import Literal

import System.Environment (getArgs)
import Control.Monad

import Outputable

main :: IO ()
main = do
  [fileName] <- getArgs
  sstgs <- do
    putStrLn . unwords $ ["Reading", fileName]
    readStgb fileName
  forM_ sstgs $ \(SStgBinding id rhs) -> do
    liftIO $ putStrLn $ showSDoc $ ppr id

testNames :: IO ()
testNames = do
  uniq <- return $ mkUnique 'c' 0
  occ <- return $ mkOccName varName "foo"
  name <- return $ mkInternalName uniq occ noSrcSpan
  sstg <- return $ SStgBinding name $ SStgRhsClosure SReEntrant [] $ SStgLit $ MachChar 'z'
  writeStgb fn [sstg, sstg]
  
  [sstg', sstg''] <- readStgb fn
  let (SStgBinding name' _) = sstg'
      (SStgBinding name'' _) = sstg''
      
  print $ name' == name''
  return ()  
  
  where fn = "/tmp/foo.stgb"
