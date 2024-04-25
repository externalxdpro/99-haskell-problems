split :: [a] -> Int -> ([a], [a])
split [] _ = ([], [])
split (x : xs) i
  | i == 0 = ([], x : xs)
  | otherwise =
      let (ys, zs) = split xs (i - 1)
       in (x : ys, zs)

main :: IO ()
main = do
  print $ split "abcdefghik" 3
