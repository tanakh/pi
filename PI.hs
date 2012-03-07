{-# LANGUAGE BangPatterns #-}
module PI (calculatePI2, calculatePI10) where

import Data.Bits
import Sqrt

a, b, c, c3, c3div24 :: Integer
a = 13591409
b = 545140134
c = 640320
c3 = c * c * c
c3div24 = c3 `div` 24

calculatePI10 :: Integer -> Integer
calculatePI10 n = (pi2 * 10 ^ n) `shiftR` (fromIntegral n2)
  where
    pi2 = calculatePI2 n2
    n2 = floor (fromIntegral n * logBase 2 10 :: Double)

calculatePI2 :: Integer -> Integer
calculatePI2 n =
  c32 * q `div` (12 * t + 12 * a * q)
  where
    (_, q, t) = compute 0 (n `div` 46)
    c32 = sqrt2 n c3

compute :: Integer -> Integer -> (Integer, Integer, Integer)
compute n1 n2
  | n2 - n1 == 1 =
    let an2 = (if even n2 then 1 else -1) * (a + b * n2)
        pn2 = (2 * n2 - 1) * (6 * n2 - 5) * (6 * n2 - 1)
        qn2 = n2 * n2 * n2 * c3div24
    in (pn2, qn2, an2 * pn2)
  | otherwise =
    let m = (n1 + n2) `shiftR` 1
        (p1, q1, t1) = compute n1 m
        (p2, q2, t2) = compute m n2
    in (p1 * p2, q1 * q2, t1 * q2 + p1 * t2)
