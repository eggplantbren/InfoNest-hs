module Main where

-- Imports
import Control.Monad (replicateM)
import Examples.Normal
import Lib
import System.Random.MWC

-- Main action
main :: IO ()
main = withSystemRandom . asGenIO $ \rng -> do
  _ <- replicateM 1000 $ singleRun myModel rng
  return ()

