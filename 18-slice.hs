slice :: [a] -> Int -> Int -> [a]
slice [] _ _ = []
slice (x : xs) i k
  | i > 1 = slice xs (i-1) (k-1)
  | k > 0 = x : slice xs 0 (k-1)
  | otherwise = []

main :: IO ()
main = do
  print $ slice "abcdefghik" 3 7
