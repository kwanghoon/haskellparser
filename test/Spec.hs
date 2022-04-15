{-# LANGUAGE PackageImports #-}

module Main where

import SyntaxCompletion (computeCand)
import SynCompInterface
import Test.Hspec

import "yapb" Config

import Data.Maybe (isJust)
import System.IO (readFile)

spec = hspec $ do
  describe "Testing examples/exp/benchmarks" $ do
    let benchmark1 = "module Benchmark1 where\n\nmain = if x "   -- examples/exp/Benchmark1.hs
    let benchmark2 = "module Benchmark2 where\n\nmain = if x < 123 then  "  -- examples/exp/Benchmark2.hs

    let config_simple = True
    let max_gs_level  = 9
    
    let config =
          Configuration
            {
              config_SIMPLE       = config_simple,
              config_R_LEVEL      = 1,
              config_GS_LEVEL     = 1,
              config_DEBUG        = False,
              config_DISPLAY      = False,
              config_PRESENTATION = 0,
              config_ALGORITHM    = 3
            }
    
    it ("[Benchmark1] " ++ benchmark1) $
      do mapM_ (item benchmark1 config) [1..max_gs_level] 

    it ("[Benchmark2] " ++ benchmark2) $
      do mapM_ (item benchmark2 config) [1..max_gs_level] 

item benchmark init_config gslevel = 
      do let test_config = init_config{config_GS_LEVEL=gslevel}
         putStrLn (show test_config)
         
         configMaybe <- readConfig
         case configMaybe of
           Just config ->
             do writeConfig test_config  -- set
                results <- computeCand False benchmark "" (config_SIMPLE test_config)
                writeConfig init_config       -- restore
                
           Nothing -> isJust configMaybe `shouldBe` True

main :: IO ()
main = spec


