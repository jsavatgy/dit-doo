import qualified Data.Map as Map

str = "mare anguis"
fileName = "journey-to-centre-of-earth.txt"
ns = [7,6..1]

chunks n xs 
  | n <= length xs = fst (splitAt n xs) : chunks n (tail xs)
  | otherwise      = []

chks str = concat [chunks x str | x <- ns]

tac a b = b ++ a

roots chks = 
  Map.fromListWith tac t
  where 
    t = [(init s, [last s]) | s <- chks]

main = do
  putStrLn "str = "
  print str
  putStrLn "roots = "
  print (roots (chks str))
