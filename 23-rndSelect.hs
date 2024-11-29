import System.Random

rndSelect :: [a] -> Int -> IO [a]
rndSelect xs n = do
  map (xs !!) . take n . randomRs (0, length xs - 1) <$> getStdGen

main = do
  rndSelect "abcdefgh" 3 >>= print
