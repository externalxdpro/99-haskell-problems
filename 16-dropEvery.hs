dropEvery :: [a] -> Int -> [a]
dropEvery xs n = dropEvery' xs n n
  where
    dropEvery' :: [a] -> Int -> Int -> [a]
    dropEvery' [] _ _ = []
    dropEvery' (x : xs) i n
      | i == 1 = dropEvery' xs n n
      | otherwise = x : dropEvery' xs (i - 1) n

main :: IO ()
main = do
  print $ dropEvery "abcdefghik" 3
