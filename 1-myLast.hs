myLast :: [a] -> a
myLast [x] = x
myLast (x : xs) = myLast xs

main :: IO ()
main = do
  print $ myLast [1, 2, 3, 4]
  print $ myLast ['x', 'y', 'z']
