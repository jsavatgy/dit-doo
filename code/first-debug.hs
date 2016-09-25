import System.Random
import qualified Data.Map as Map

ns = [7,6..1]

chunks n xs 
  | n <= length xs = fst (splitAt n xs) : chunks n (tail xs)
  | otherwise      = []

intToDouble :: Int -> Double
intToDouble = fromRational . toRational

rootmap str = 
  Map.fromListWith (Map.unionWith (+)) t
  where 
    t = [(init s, Map.singleton (last s) 1) | s <- chks str]
    chks str = concat [chunks x str | x <- ns]

mapAccumFsum = Map.mapAccum fsum 0 
  where
    fsum a b = (a + b, (a+1,a+b))

scale :: Int -> Double -> Int
scale i r = min i t
  where
    itd = intToDouble i
    t = floor (r*(itd+1))

rands :: IO [Double]
rands = do
  g <- getStdGen
  let rs = randomRs (0.0,1.0) g 
  return rs

lastN n xs = reverse (take n (reverse xs))
floorLogBase = floor . logBase 0.5
flipTrunc a b x = if x > b then a else a `max` x 
filterMe (d,m) c = Map.filter (\(a,b) -> a<=c && c<=b) m

randLength rnd = (flipTrunc 0 6) (floorLogBase rnd)

randAccum rmap x out = findAccum rmap (drop out x)
findAccum rmap x = lookAccum rmap (Map.lookup x rmap) x
lookAccum rmap (Just x) str = (str,x)
lookAccum rmap Nothing  str = lookAccum rmap (Map.lookup next rmap) next
  where
    next = drop 1 str

extTuple tree rndout rand accum = ((accum, l, rndout, limit, scaledRand), ks)
  where
    l = length accum
    ks = Map.keys (filterMe (limit,i) scaledRand)
    (limit,i) = mapAccumFsum tree
    scaledRand = (scale (limit-1) rand) + 1

accumLen = 6
    
genAccum rmap rands accum = 
  accTuple : genAccum rmap newRands newAccum
  where
    newRands = drop 2 rands
    newAccum = lastN accumLen (accum ++ c)
    (_,c) = accTuple
    rndout = randLength (rands!!0)
    (accu,tree) = randAccum rmap accum rndout
    accTuple = extTuple tree rndout (rands!!1) accu

main = do
  rs <- rands
  --content <- readFile "matkustus-maan-keskipisteeseen.txt"
  content <- readFile "journey-to-centre-of-earth.txt"
  let 
    example = take 10000 content
    rmap = rootmap example
    accum = take accumLen example
    accums = genAccum rmap rs accum
  mapM_ (putStrLn . show) (take 55 accums)

