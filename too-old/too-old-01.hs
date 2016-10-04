import System.Random
import System.IO (hFlush, stdout)
import Data.Maybe (fromJust)
import Data.List.Split (chunksOf)
import Data.Map (Map)
import qualified Data.Map as Map
import qualified System.IO.Strict as S

accumF a b = (a + b, a + b)
ns = [7,6..1]
lastTplLst s = Map.singleton (last s) 1
lastN n xs = reverse (take n (reverse xs))
fileName = "viisi-viikkoa-ilmapallossa.txt"

chunks n xs 
  | n <= length xs = fst (splitAt n xs) : chunks n (tail xs)
  | otherwise      = []

nth rand r (x:y:zs)
  | r <= rand = x
  | otherwise = nth rand (r*r) (y:zs)
nth rand r (x:[]) = x

testSample sampleWrds str =
  Map.lookup str sampleMap
  where
    sample = zip (map init sampleWrds) (map lastTplLst sampleWrds)
    sampleMap = Map.fromListWith (Map.unionWith (+)) sample

intToDouble :: Int -> Double
intToDouble i = fromRational (toRational i)

frekratio x y =
  intToDouble x / intToDouble y

growing xs = 
  zip b [sum (take n c) | n <- [1..length c]]
  where
    (b,c) = unzip xs

newLetter rands res7 =
  fst (head dw) 
  where
    fj = fromJust (nth r2 0.50 [x | x <- res7, x /= Nothing])
    xs = Map.toList fj
    frekSum = sum [snd x | x <- xs]
    --(frekSum, accumTree) = Map.mapAccum accumF 0 fj 
    --accumLst = Map.toList accumTree
    --dw = dropWhile (\x -> frekratio (snd x) frekSum < r1) accumLst
    dw = dropWhile (\x -> frekratio (snd x) frekSum < r1) (growing xs)

    r1 = head rands
    r2 = head (drop 1 rands)

{-
# a typical res7:
Nothing
Nothing
Nothing
Just (fromList [('I',1)])
Just (fromList [(' ',2),('A',1),('I',2),('S',1),('a',14),('c',1),('e',22),
  ('i',6),('k',3),('o',2),('p',1),('t',1),('u',7)])
Just (fromList [('\n',411),('\r',411),(' ',2047),('!',19),('"',25),('\'',14),
  ('(',15),(')',19),(',',230),('-',102),('.',167),('0',9),('1',13),('2',6),
  ('3',4),('4',4),('5',9),('6',3),('7',2),('8',7),('9',1),(':',23),(';',21),
  ('=',2),('?',2),('A',20),('B',12),('C',8),('D',12),('E',37),('F',35),
  ('G',7),('H',20),('I',19),('J',13),('K',38),('L',24),('M',42),('N',16),
  ('O',7),('P',20),('R',10),('S',63),('T',35),('U',10),('V',10),('W',3),
  ('Y',4),('Z',3),('_',34),('a',1763),('b',16),('c',31),('d',113),('e',1393),
  ('f',21),('g',65),('h',374),('i',1775),('j',238),('k',809),('l',937),
  ('m',468),('n',1615),('o',823),('p',236),('q',1),('r',438),('s',1203),
  ('t',1446),('u',708),('v',304),('w',4),('x',3),('y',278),('z',3),
  ('\156',1),('\196',2),('\228',796),('\233',1),('\235',1),('\246',64),
  ('\252',1),('\65279',1)])
-}

generate sample7s accum rands = 
  c : generate sample7s newAccum newRands
  where
    newRands = drop 2 rands
    newAccum = lastN 6 (accum ++ [c])
    res7 = [testSample s (lastN (n-1) accum) | (s,n) <- zip sample7s ns]
    c = newLetter (take 2 rands) res7

putCharF c = do
  (putStr . charToString) c
  hFlush stdout

cutGenerate piece accum = do
  setStdGen (mkStdGen 2015) -- test case
  g <- getStdGen
  let 
    rands = randomRs (0.0,1.0) g :: [Double]
    sample7s = [chunks n piece | n <- ns]
  --mapM_ putCharF (generate sample7s accum rands)
  putStrLn (take 3000 (generate sample7s accum rands))
  --putStrLn ""

charToString :: Char -> String
charToString c = [c]

main = do
  --content <- readFile fileName
  content <- S.readFile fileName
  let
    startAccum = take 6 content
    pieces = chunksOf 20000 content
    piece = concat (take 1 pieces)
  mapM_ putCharF startAccum
  cutGenerate piece startAccum

