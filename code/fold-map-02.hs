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

treeV r = Map.showTreeWith (\k x -> show (k,x)) True False r

filterMe (d,m) c = Map.filter (\(a,b) -> a<=c && c<=b) m

main = do
  putStrLn "rootmap = "
  let r = rootmap "mare imbrium"
  print r
  putStrLn "treeV = "
  putStrLn (treeV r)
  let lu = r Map.! ""
  putStrLn "lu = "
  print lu
  --let ma = Map.mapAccum fsum 0 lu
  let ma = mapAccumFsum lu
  putStrLn " ma = "
  print ma
  let fm = map (Map.keys . filterMe ma) [1..15]
  putStrLn "fm = "
  print fm

