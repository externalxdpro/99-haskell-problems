removeAt :: Int -> [a] -> (Maybe a, [a])
removeAt _ [] = (Nothing, [])
removeAt 0 (x : xs) = (Just x, snd (removeAt (-1) xs))
removeAt i (x : xs) =
  let (y, ys) = removeAt (i - 1) xs
   in (y, x : ys)

main = do
  print $ removeAt 2 "abcd"
