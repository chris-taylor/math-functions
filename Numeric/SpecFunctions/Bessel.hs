-- |
-- Module    : Numeric.SpecFunctions.Bessel
-- Copyright : (c) 2013 Chris Taylor
-- License   : BSD3
--
-- Maintainer  : bos@serpentine.com
-- Stability   : experimental
-- Portability : portable
--
-- Numerical computation of Bessel functions.
module Numeric.MathFunctions.Bessel (
    j0
  , j1
  ) where

j0 :: Double -> Double
j0 = undefined

j1 :: Double -> Double
j1 = undefined

-- References:

-- Fast and Accurate Bessel Function Computation
-- http://www.cl.cam.ac.uk/~jrh13/papers/bessel.html
-- Abstract: The Bessel functions are considered relatively difficult to
--   compute. Although they have a simple power series expansion that is
--   everywhere convergent, they exhibit approximately periodic behavior
--   which makes the direct use of the power series impractically slow and
--   numerically unstable. We describe an alternative method based on
--   systematic expansion around the zeros, refining existing techniques
--   based on Hankel expansions, which mostly avoids the use of multiprecision
--   arithmetic while yielding accurate results.