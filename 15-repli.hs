repli :: [a] -> Int -> [a]
repli [] _ = []
repli (x : xs) i = [x | _ <- [1 .. i]] ++ repli xs i

main :: IO ()
main = do
  print $ repli "abc" 3
