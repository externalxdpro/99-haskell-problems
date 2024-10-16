rotate :: [a] -> Int -> [a]
rotate [] _ = []
rotate (x : xs) i
  | i > 0 = rotate (xs ++ [x]) (i - 1)
  | i < 0 = rotate (last xs : x : init xs) (i + 1)
  | i == 0 = x : xs

main = do
  print $ rotate "abcdefgh" 3
  print $ rotate "abcdefgh" (-2)
