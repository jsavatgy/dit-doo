
str = "sinus aestuum"
ns = [7,6..1]

chunks n xs 
  | n <= length xs = fst (splitAt n xs) : chunks n (tail xs)
  | otherwise      = []

chks = [chunks x str | x <- ns]

main = do
  putStrLn "str = "
  print str
  putStrLn "chks = "
  mapM_ (putStrLn . show) chks

