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
  -- * Bessel functions of the first kind
    j0
  , j1
  , besselj
  -- * Bessel functions of the second kind
  , y0
  , y1
  , bessely
  -- * Modified Bessel functions of the first kind
  , i0
  , i1
  , besseli
  -- * Modified Bessel functions of the second kind
  , k0
  , k1
  , besselk
  ) where

import Numeric.MathFunctions.Constants

-- Bessel functions of the first kind, J_n(z)

j0 :: Double -> Double
j0 = undefined

j1 :: Double -> Double
j1 = undefined

besselj :: Int -> Double -> Double
besselj n x
  | n < 0     = (if odd n then -1 else 1) * worker (abs n) x
  | otherwise = worker n x
 where
  worker n x = undefined



-- Bessel functions of the second kind, Y_n(z)

y0 :: Double -> Double
y0 = undefined

y1 :: Double -> Double
y1 = undefined

bessely :: Int -> Double -> Double
bessely n x
  | n < 0     = (if odd n then -1 else 1) * worker (abs n) x
  | otherwise = worker n x
 where
  worker n x = undefined



-- Modified Bessel functions of the first kind, I_n(z)

-- | Computes I_0(z), the zeroth order modified Bessel function of
-- the first kind.
i0 :: Double -> Double
i0 = besseli 0.0

-- | Computes I_1(x), the first order modified Bessel function of
-- the first kind.
i1 :: Double -> Double
i1 = besseli 1.0

-- | Computes I_n(z), the modified Bessel function of the first kind.
besseli :: Double -> Double -> Double
besseli = undefined



-- Modified Bessel functions of the second kind, K_n(z)

-- | Computes K_0(z), the zeroth order modified Bessel function
-- of the second kind.
k0 :: Double -> Double
k0 = besselk 0.0

-- | Computes K_1(z), the first order modified Bessel function of the
-- second kind.
k1 :: Double -> Double
k1 = besselk 1.0

-- | Computes K_n(z), the modified Bessel function of the second kind.
besselk :: Double -> Double -> Double
besselk = besselkHelper (1/32)

-- | The workhorse for computing K_n(z).
besselkHelper :: Double -> Double -> Double -> Double
besselkHelper h n z
  | z == 0.0  = m_pos_inf
  | otherwise = go s0 h
 where
  go :: Double -> Double -> Double
  go s t
    | term / s < 1e-20 = h * (s + term) -- may as well add the last term
    | otherwise        = go (s + term) (t + h)
   where
    term = cosh (n * t) * exp (-z * cosh t)

  s0 = 0.5 * exp (-z)



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

-- Numerical Calculation of Bessel functions
-- http://socrates.berkeley.edu/~schwrtz/PhysicsPapers/55:IJMPC23_1250084.pdf
-- Abstract: A new computation procedure is offered to provide simple, accurate
--   and flexible methods for using modern computers to give numerical
--   evaluations of the various Bessel functions. The trapezoidal rule, applied
--   to suitable integral representations, may become the method of choice for
--   evaluation of the many special functions of mathematical physics.