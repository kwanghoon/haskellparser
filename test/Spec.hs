{-# LANGUAGE PackageImports #-}

module Main where

import SyntaxCompletion (computeCand)
import SynCompInterface
import Test.Hspec

import "yapb" Config

import Data.Maybe (isJust)
import System.IO (readFile)

spec = hspec $ do
  describe "Haskell" $ do

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
    
    let benchmark1_text = "module Benchmark1 where\n\nmain = if x "   -- examples/exp/Benchmark1.hs
    let benchmark2_text = "module Benchmark2 where\n\nmain = if x < 123 then  "  -- examples/exp/Benchmark2.hs

    let benchmark1 = "./examples/exp/Benchmark1.hs"
    
    it ("[Benchmark1] ") $
      do mapM_ (itemText benchmark1_text config) [1..max_gs_level]  -- Max GS level (9) for Smallbasic

    let benchmark2 = "./examples/exp/Benchmark2.hs"
    
    it ("[Benchmark2] ") $
      do mapM_ (itemText benchmark2_text config) [1..max_gs_level]  -- Max GS level (9) for Smallbasic

    let benchmark3 = "./examples/exp/Benchmark3.hs"

    it ("[Benchmark3] ") $
      do mapM_ (item benchmark3 benchmark1_text config) [1..max_gs_level]  -- Max GS level (9) for Smallbasic

    let benchmark4 = "./examples/exp/Benchmark4.hs"
    
    it ("[Benchmark4] ") $
      do mapM_ (item benchmark4 benchmark2_text config) [1..max_gs_level]  -- Max GS level (9) for Smallbasic


item benchmark_file text init_config gslevel = 
      do let test_config = init_config{config_GS_LEVEL=gslevel}
         putStrLn (show test_config)
         
         configMaybe <- readConfig
         benchmark_prefix <- readFile benchmark_file
         let benchmark = benchmark_prefix ++ text
         case configMaybe of
           Just _ ->
             do writeConfig test_config  -- set
                results <- computeCand False benchmark "" (config_SIMPLE test_config)
                writeConfig init_config       -- restore
                
           Nothing -> isJust configMaybe `shouldBe` True

itemText benchmark init_config gslevel = 
      do let test_config = init_config{config_GS_LEVEL=gslevel}
         putStrLn (show test_config)
         
         configMaybe <- readConfig
         -- benchmark <- readFile benchmark_file
         case configMaybe of
           Just _ ->
             do writeConfig test_config  -- set
                results <- computeCand False benchmark "" (config_SIMPLE test_config)
                writeConfig init_config       -- restore
                
           Nothing -> isJust configMaybe `shouldBe` True

main :: IO ()
main = spec


