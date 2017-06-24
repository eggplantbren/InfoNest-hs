{-# LANGUAGE RecordWildCards #-}

module Lib where

-- Imports
import Control.Monad (when)
import Control.Monad.Primitive
--import qualified Data.Vector as V
import System.Random.MWC

-- a is the type of points in the (parameter x data) space
data Model a = Model {

                 -- Generate parameters and data from
                 -- the joint prior
                 generate :: Gen RealWorld -> IO a,

                 -- Metropolis proposal *of the parameters*
                 perturb  :: a -> Gen RealWorld -> IO (Double, a),

                 -- Log Likelihood function
                 logLikelihood :: a -> Double,

                 -- Distance function
                 distance :: a -> a -> Double,

                 -- Particle to string
                 toString :: a -> String
               }

-- A type for a threshold
data Threshold a = None | Threshold a Double

-- Perform a single iteration of the algorithm.
singleRun :: Model a -> Gen RealWorld -> IO ()
singleRun Model {..} rng = do

  -- Generate the reference point
  referencePoint <- generate rng
  let referenceLogl = logLikelihood referencePoint

  -- Do some MCMC
  _ <- doMetropolis (0, 1000000)
                    (referencePoint, referenceLogl)
                    None
                    Model {..}
                    rng

  return ()



doMetropolis :: (Int, Int)      -- (i, steps)
             -> (a, Double)     -- (particle, log likelihood)
             -> Threshold a     -- Distance threshold
             -> Model a         -- Model specification
             -> Gen RealWorld   -- RNG
             -> IO (a, Double)  -- Updated particle and log likelihood
doMetropolis (i, steps) (particle, logl) threshold Model {..} rng
  | i >= steps = return (particle, logl)
  | otherwise  = do
                   -- Proposal
                   (logH, particle') <- perturb particle rng
                   let logl' = logLikelihood particle'

                   -- Acceptance probability
                   let logAlpha = logH + logl' - logl
                   let alpha' = if logAlpha > 0.0
                                then 1.0
                                else exp logAlpha

                   -- Test for acceptance
                   let aboveThreshold = case threshold of
                                None -> True
                                Threshold refParticle value ->
                                  distance particle' refParticle <= value

                   u <- uniform rng :: IO Double
                   let result = if aboveThreshold && u < alpha' 
                                then (particle', logl')
                                else (particle, logl)

                   -- Print message
                   when (mod (i+1) 1000 == 0) $
                     putStrLn $ show (i+1) ++ " " ++ toString particle

                   -- Continue
                   doMetropolis (i+1, steps) result threshold Model {..} rng

