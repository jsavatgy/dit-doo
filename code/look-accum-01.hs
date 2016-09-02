import System.Random
import qualified Data.Map as Map

ns = [7,6..1]

chunks n xs 
  | n <= length xs = fst (splitAt n xs) : chunks n (tail xs)
  | otherwise      = []

rootmap str = 
  Map.fromListWith (Map.unionWith (+)) t
  where 
    t = [(init s, Map.singleton (last s) 1) | s <- chks str]
    chks str = concat [chunks x str | x <- ns]

mapAccumFsum = Map.mapAccum fsum 0 
  where
    fsum a b = (a + b, (a+1,a+b))

rands :: IO [Double]
rands = do
  g <- getStdGen
  let rs = randomRs (0.0,1.0) g 
  return rs

floorLogBase = floor . logBase 0.5
truncBetween a b x = a `max` x `min` b
filterMe (d,m) c = Map.filter (\(a,b) -> a<=c && c<=b) m

randLts rands = map (truncBetween 0 6) r1s
  where
    r1s = map floorLogBase rands

randAccum rmap (x,out) = findAccum rmap (drop out x)
findAccum rmap x = lookAccum rmap (Map.lookup x rmap) x
lookAccum rmap (Just x) str = (str,x)
lookAccum rmap Nothing  str = lookAccum rmap (Map.lookup next rmap) next
  where
    next = drop 1 str

example = "lacus veris"

main = do
  rs <- rands
  let 
    rmap = rootmap example
    examples = chunks 6 example
    rl = randLts rs
    l1 = map (findAccum rmap) examples
    l2 = map (randAccum rmap) (zip examples rl)
  mapM_ (putStrLn . show) l1
  putStrLn ""
  mapM_ (putStrLn . show) l2

