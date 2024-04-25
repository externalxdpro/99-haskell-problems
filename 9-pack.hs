pack :: (Eq a) => [a] -> [[a]]
pack xs = process xs []
  where
    process :: (Eq a) => [a] -> [[a]] -> [[a]]
    process [] out = reverse out
    process (x : xs) [] = process xs [[x]]
    process (x : xs) (ox : oxs)
      | x == head ox = process xs ((x : ox) : oxs)
      | otherwise = process xs ([x] : (ox : oxs))

main :: IO ()
main = do
  print $
    pack
      [ 'a',
        'a',
        'a',
        'a',
        'b',
        'c',
        'c',
        'a',
        'a',
        'd',
        'e',
        'e',
        'e',
        'e'
      ]
