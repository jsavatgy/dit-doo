import qualified Data.Map as Map

fileName = "journey-to-centre-of-earth.txt"
ns = [7,6..1]

chunks n xs 
  | n <= length xs = fst (splitAt n xs) : chunks n (tail xs)
  | otherwise      = []

rootmap str = 
  Map.fromListWith (Map.unionWith (+)) t
  where 
    t = [(init s, Map.singleton (last s) 1) | s <- chks str]
    chks str = concat [chunks x str | x <- ns]

tests = chunks 3 "pqrstuvwxyz"
partOfContent = take 10000
testLookup str = map (\s -> Map.lookup s (rootmap str)) tests
treeV r = Map.showTreeWith (\k x -> show (k,x)) True False r

main = do
  putStrLn "rootmap = "
  let r = rootmap "lacus odii"
  print r
  putStrLn "treeV = "
  putStrLn (treeV r)
  putStrLn "tst = "
  content <- readFile fileName
  let tst = testLookup (partOfContent content)
  print tst

