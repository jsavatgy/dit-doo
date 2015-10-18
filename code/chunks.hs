
chunks n xs 
  | n <= length xs = fst (splitAt n xs) : chunks n (tail xs)
  | otherwise      = []


chks str = [chunks n str | n <- ns]
ns = [7,6..1]
str = "mare imbrium"

main = do
  putStrLn "str = "
  print str
  putStrLn "(chks str) ="
  mapM_ (putStrLn . show) (chks str)


