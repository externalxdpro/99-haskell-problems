data ListItem a = Single a | Multiple Int a
  deriving (Show)

pack :: (Eq a) => [a] -> [[a]]
pack xs = process xs []
  where
    process :: (Eq a) => [a] -> [[a]] -> [[a]]
    process [] out = reverse out
    process (x : xs) [] = process xs [[x]]
    process (x : xs) (ox : oxs)
      | x == head ox = process xs ((x : ox) : oxs)
      | otherwise = process xs ([x] : (ox : oxs))

encodeModified :: (Eq a) => [a] -> [ListItem a]
encodeModified =
  map
    ( \x ->
        let len = length x
         in if len == 1
              then
                Single (head x)
              else
                Multiple len (head x)
    )
    . pack

main :: IO ()
main = do
  print $ encodeModified "aaaabccaadeeee"
