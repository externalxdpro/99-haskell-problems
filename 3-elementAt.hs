elementAt :: [a] -> Int -> a
elementAt (x : xs) 1 = x
elementAt (x : xs) i = elementAt xs (i - 1)

main :: IO ()
main = do
  print $ elementAt [1, 2, 3] 2
  print $ elementAt "haskell" 5
