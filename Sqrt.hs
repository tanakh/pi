{-# LANGUAGE BangPatterns #-}
module Sqrt (
  sqrt2, sqrt10,
  rsqrt2, rsqrt10,
  ) where

import Data.Bits

sqrt10 :: Integer -> Integer -> Integer
sqrt10 d n = (s2 * 10 ^ d) `shiftR` fromIntegral d2 where
  d2 = floor (fromIntegral d * logBase 2 10 :: Double)
  s2 = sqrt2 d2 n

rsqrt10 :: Integer -> Integer -> Integer
rsqrt10 = undefined

sqrt2 :: Integer -> Integer -> Integer
sqrt2 d n = n * rsqrt2 d n

rsqrt2 :: Integer -> Integer -> Integer
rsqrt2 d n 
  | d < 8 = floor $ approx * 2 ^ d
  | otherwise = iter 8 $ (floor $ approx * 2 ^ initd) `shiftL` (di - initd)
  where
    di = fromInteger d
    initd = min 300 di
    approx = 1.0 / sqrt (fromIntegral n :: Double)
    mult x y = (x * y) `shiftR` di
    iter :: Integer -> Integer -> Integer
    iter !p !x
      | p >= d = x
      | otherwise = iter (p*2) ((3 * x - n * x `mult` x `mult` x) `shiftR` 1)
