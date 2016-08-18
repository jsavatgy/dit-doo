import Data.Map
import Data.List
import Data.Ord

str = "sinus aestuum"
fileName = "journey-to-centre-of-earth.txt"

frequency content = 
  (toList . fromListWith (+)) t
  where 
    t = [(c, 1) | c <- content]

sortBySnd =  reverse . sortBy (comparing snd)
fr = frequency str

main = do
  print [(c, 1) | c <- str]

  putStrLn "frequency str = "
  print fr

  print (sortBySnd fr)

  content <- readFile fileName
  putStrLn "frequency content = "
  print (frequency content)

