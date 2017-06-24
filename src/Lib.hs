module Lib where

-- Imports
import Control.Monad.Primitive
import System.Random.MWC

-- a is the type of points in the (parameter x data) space
data Model a = Model {

                 -- Generate parameters and data from
                 -- the joint prior
                 generate :: Gen RealWorld -> IO a
               }

