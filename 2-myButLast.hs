myButLast :: [a] -> a
myButLast [x, y] = x
myButLast (x : xs) = myButLast xs

main :: IO ()
main = do
  print $ myButLast [1, 2, 3, 4]
  print $ myButLast ['a' .. 'z']
