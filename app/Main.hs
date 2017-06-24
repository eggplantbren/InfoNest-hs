module Main where

-- Imports
import Control.Monad.Primitive
import qualified Data.Vector.Unboxed as U
import Lib
import System.Random.MWC
import System.Random.MWC.Distributions

-- An example particle type
data MyParticle = MyParticle {
                    myParticleMu :: !(Double),
                    myParticleXs :: !(U.Vector Double)
                  } deriving Show

-- Generate function
myGenerate :: Gen RealWorld -> IO MyParticle
myGenerate rng = do
  mu <- (*10.0) <$> standard rng
  ns <- U.replicateM 10 (standard rng)
  let xs = U.map (+mu) ns
  return $ MyParticle mu xs

-- Generate function
myPerturb :: MyParticle -> Gen RealWorld -> IO (Double, MyParticle)
myPerturb (MyParticle mu xs) rng = do
  -- Proposal scale
  scale <- (\x -> 10.0**(3.0*x)) <$> standard rng
  mu' <- (\n -> mu + scale*n) <$> standard rng
  let logH = -0.5*(mu' / 10.0)**2 + 0.5*(mu / 10.0)**2
  return $! (logH, MyParticle mu' xs)

-- Log likelihood
myLogLikelihood :: MyParticle -> Double
myLogLikelihood (MyParticle mu xs) = -0.5*theSum where
  theSum = U.foldl' (\acc x -> acc + (x - mu)**2) 0.0 xs

myModel :: Model MyParticle
myModel = Model myGenerate myPerturb myLogLikelihood

main :: IO ()
main = withSystemRandom . asGenIO $ \rng -> do
  particle <- myGenerate rng
  print particle
  print $ myLogLikelihood particle

  (logH, particle') <- myPerturb particle rng
  print logH
  print particle'


