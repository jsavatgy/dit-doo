import Data.Map

str = "sinus aestuum"
fileName = "journey-to-centre-of-earth.txt"

frequency content = 
  (toList . fromListWith (+)) t
  where 
    t = [(c, 1) | c <- content]

main = do
  print [(c, 1) | c <- str]

  putStrLn "frequency str = "
  print (frequency str)

  content <- readFile fileName
  putStrLn "frequency content = "
  print (frequency content)

