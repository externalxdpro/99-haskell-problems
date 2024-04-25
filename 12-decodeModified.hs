data ListItem a = Single a | Multiple Int a
  deriving (Show)

decodeModified :: [ListItem a] -> [a]
decodeModified xs = process xs []
  where
    process :: [ListItem a] -> [a] -> [a]
    process [] out = reverse out
    process (Single x : xs) out = process xs (x : out)
    process (Multiple i x : xs) out = process xs ([x | _ <- [1 .. i]] ++ out)

main :: IO ()
main = do
  print $
    decodeModified
      [ Multiple 4 'a',
        Single 'b',
        Multiple 2 'c',
        Multiple 2 'a',
        Single 'd',
        Multiple 4 'e'
      ]
