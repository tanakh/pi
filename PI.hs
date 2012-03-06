{-# LANGUAGE BangPatterns #-}
module Main where

import System.Environment
import Debug.Trace

main :: IO ()
main = do
  args <- getArgs
  case args of
    [n, d] -> do
      print $ sqrt' (read n) (read d)
    [n] -> print $ calculatePI $ read n
    _ -> putStrLn "usage: <number-of-digit>"

calculatePI :: Integer -> Integer
calculatePI n = -- traceShow (p, q, t, c32) $
  c32 * q `div` (12 * t + 12 * a * q)
  where
    (p, q, t) = compute 0 (n `div` 14)
    c32 = sqrt' n (c^3)

a, b, c :: Integer
a = 13591409
b = 545140134
c = 640320

compute :: Integer -> Integer -> (Integer, Integer, Integer)
compute n1 n2
  | n2 - n1 == 1 =
    let an2 = (if even n2 then 1 else (-1)) * (a + b * n2)
        pn2 = (2 * n2 - 1) * (6 * n2 - 5) * (6 * n2 - 1)
        qn2 = n2 ^ 3 * c ^ 3 `div` 24
    in (pn2, qn2, an2 * pn2)
  | otherwise =
    let m = (n1 + n2) `div` 2
        (p1, q1, t1) = compute n1 m
        (p2, q2, t2) = compute m n2
    in (p1 * p2, q1 * q2, t1 * q2 + p1 * t2)

sqrt' :: Integer -> Integer -> Integer
sqrt' d n = n * rsqrt d n

rsqrt :: Integer -> Integer -> Integer
rsqrt d n 
  | d < 8 = floor $ approx * 10 ^ d
  | otherwise = iter 1 $ (floor $ approx * 10 ^ initd) * 10 ^ (d - initd)
  where
    initd = min 100 d
    approx = 1.0 / sqrt (fromIntegral n :: Double)
    mult x y = x * y `div` 10 ^ d
    iter :: Integer -> Integer -> Integer
    iter !p !x
      | p >= d = x
      | otherwise = iter (p*2) ((3 * x - n * x `mult` x `mult` x) `div` 2)
