import Criterion.Main
import PI

main :: IO ()
main = defaultMain
  [ bench "pi" $ nf calculatePI10 100000
  ]
