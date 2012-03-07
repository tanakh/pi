module Main where

import System.Environment
import PI
import Sqrt

main :: IO ()
main = do
  args <- getArgs
  case args of
    [n] -> do
      -- print $ sqrt10 (read n) 2
      -- print $ calculatePI2 $ read n
      print $ calculatePI10 $ read n
    _ -> do
      putStrLn "usage: <number-of-digit>"
