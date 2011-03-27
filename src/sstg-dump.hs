module Main where

import Language.SSTG.Syntax
import Language.SSTG.Serialization

import System.Environment (getArgs)
import Control.Applicative
import Control.Monad

import Outputable

main = do
  stgbs <- getArgs
  groups <- concat <$> mapM readStgb stgbs
  
  forM_ groups $ \group -> do
    forM group $ \(SStgBinding name _) ->
      putStrLn $ showSDoc $ ppr name
