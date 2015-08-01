
str = "sinus aestuum"

chunks n xs 
  | n <= length xs = fst (splitAt n xs) : chunks n (tail xs)
  | otherwise      = []

chks = [chunks x str | x <- [7,6..1]]

main = do
  mapM_ print chks

