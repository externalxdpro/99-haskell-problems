data ListItem a = Single a | Multiple Int a
  deriving (Show)

-- encodeDirect :: (Eq a) => [a] -> [ListItem a]
-- encodeDirect xs = process xs []
--   where
--     process :: (Eq a) => [a] -> [ListItem a] -> [ListItem a]
--     process [] out = reverse out
--     process (x : xs) [] = process xs [Single x]
--     process (x : xs) (Single ox : oxs)
--       | x == ox = process xs (Multiple 2 x : oxs)
--       | otherwise = process xs (Single x : Single ox : oxs)
--     process (x : xs) (Multiple i ox : oxs)
--       | x == ox = process xs (Multiple (i + 1) x : oxs)
--       | otherwise = process xs (Single x : Multiple i ox : oxs)

encodeDirect :: (Eq a) => [a] -> [ListItem a]
encodeDirect xs = clean $ process xs []
  where
    process :: (Eq a) => [a] -> [ListItem a] -> [ListItem a]
    process [] out = reverse out
    process (x : xs) [] = process xs [Multiple 1 x]
    process (x : xs) (Multiple i ox : oxs)
      | x == ox = process xs (Multiple (i + 1) x : oxs)
      | otherwise = process xs (Multiple 1 x : Multiple i ox : oxs)

    clean :: [ListItem a] -> [ListItem a]
    clean [] = []
    clean (Multiple i x : xs)
      | i == 1 = Single x : clean xs
      | otherwise = Multiple i x : clean xs

main :: IO ()
main = do
  print $ encodeDirect "aaaabccaadeeee"
