range :: Int -> Int -> [Int]
range x y =
  if x == y
    then [x]
    else x : range (x + 1) y

main = do
  print $ range 4 9
