{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RecordWildCards #-}

module Lib where

-- Imports
import Control.Monad.Primitive
import System.Random.MWC
import System.IO

-- There is 'standard' mode and 'conditional entropy' mode
data Mode = Standard | Conditional

-- a is the type of points in the (parameter x data) space
data Model a = Model {

                 -- What mode is appropriate?
                 mode     :: Mode,

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

-- Sampler options
data SamplerOptions = SamplerOptions {
                        numParticles :: !Int,
                        mcmcSteps    :: !Int
                      }

-- A type for a threshold
data Threshold a = None | Threshold a Double

-- Perform a single iteration of the algorithm.
singleRun :: Model a -> Gen RealWorld -> IO ()
singleRun Model {..} rng = do

  -- Generate the reference point
  referencePoint <- generate rng
  let referenceLogl = logLikelihood referencePoint

  -- Do some MCMC to generate a new point from the posterior
  let genParticle = doMetropolis (0, 10000)
                        (referencePoint, referenceLogl)
                        None
                        Model {..}
                        rng -- :: IO (a, Double)

  (particle, logl) <- genParticle

  -- Do Nested Sampling
  _ <- doNestedSampling (particle, logl) referencePoint Model {..} rng

  return ()


doNestedSampling :: (a, Double)   -- (particle, log likelihood)
                 -> a             -- Reference particle
                 -> Model a       -- Model specification
                 -> Gen RealWorld -- RNG
                 -> IO ()
doNestedSampling !(particle, logl) reference Model {..} rng = do
  let dist = distance particle reference

  if dist < 1E-4
  then do
    return ()
  else do
    print dist
    hFlush stdout

    let threshold = Threshold reference dist
    (particle', logl') <- doMetropolis (0, 10000)
                                       (particle, logl)
                                       threshold
                                       Model {..}
                                       rng

    doNestedSampling (particle', logl') reference Model {..} rng


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
                   let logAlpha = case mode of
                                    Standard    -> logH
                                    Conditional -> logH + logl' - logl

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

                   -- Continue
                   doMetropolis (i+1, steps) result threshold Model {..} rng

