module Main where

-- Imports
import Control.Monad.Primitive
import qualified Data.Vector.Unboxed as U
import Lib
import System.Random.MWC
import System.Random.MWC.Distributions

-- An example particle type
data MyParticle = MyParticle {
                    myParticleMu :: Double,
                    myParticleXs :: U.Vector Double
                  } deriving Show

-- Generate function
myGenerate :: Gen RealWorld -> IO MyParticle
myGenerate rng = do
  mu <- (*10.0) <$> standard rng
  ns <- U.replicateM 10 (standard rng)
  let xs = U.map (+mu) ns
  return $ MyParticle mu xs

myModel :: Model MyParticle
myModel = Model myGenerate 

main :: IO ()
main = withSystemRandom . asGenIO $ \rng -> do
  particle <- myGenerate rng
  print particle

