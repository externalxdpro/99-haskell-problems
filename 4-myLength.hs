-- myLength :: [a] -> Int
-- myLength xs = myLength' xs 0
--   where
--     myLength' :: [a] -> Int -> Int
--     myLength' [] i = i
--     myLength' (x : xs) i = myLength' xs (i + 1)

-- myLength :: [a] -> Int
-- myLength [] = 0
-- myLength (x : xs) = 1 + myLength xs

myLength :: [a] -> Int
myLength = foldr (\x -> (+) 1) 0

main :: IO ()
main = do
  print $ myLength [123, 456, 789]
  print $ myLength "Hello, world!"
