import qualified Data.Map as Map

str = "mare anguis"
fileName = "journey-to-centre-of-earth.txt"
ns = [7,6..1]

chunks n xs 
  | n <= length xs = fst (splitAt n xs) : chunks n (tail xs)
  | otherwise      = []

chks str = concat [chunks x str | x <- ns]

tac a b = b ++ a

exts exs = 
  (Map.toList . Map.fromListWith (+)) t
  where 
    t = [(s,1) | s <- exs]

roots chks = 
  Map.fromListWith tac t
  where 
    t = [(init s, [last s]) | s <- chks]

mroots s = Map.map exts (roots (chks s))
g str = mroots str
tests = chunks 3 "mnopqrstuvwxyz"
l str = map (\s -> Map.lookup s (g str)) tests

main = do
  content <- readFile fileName
  putStrLn "roots = "
  print (g str)
  print (l (take 12000 content))

