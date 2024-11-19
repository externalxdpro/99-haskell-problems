insertAt :: a -> [a] -> Int -> [a]
insertAt y xs 1 = y : xs
insertAt y (x : xs) i = x : insertAt y xs (i - 1)

main = do
  print $ insertAt 'X' "abcd" 2
