isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome xs = myReverse xs == xs
  where
    myReverse :: [a] -> [a]
    myReverse [] = []
    myReverse (x : xs) = myReverse xs ++ [x]

main :: IO ()
main = do
  print $ isPalindrome [1, 2, 3]
  print $ isPalindrome "madamimadam"
  print $ isPalindrome [1, 2, 4, 8, 16, 8, 4, 2, 1]
