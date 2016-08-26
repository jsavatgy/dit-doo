import Data.Maybe (fromJust)
import Data.Map hiding (lookup)
import Data.List
import Data.List.Split (chunksOf)
import System.Random
import System.IO (hFlush, stdout)


lastN n xs = reverse (take n (reverse xs))
found str fks = [lookup (lastN n str) f  | (n,f) <- zip [6,5..0] fks]

initchks chks = [(init c, [last c]) | c <- chks]

freks2 fks = toList (fromListWith (++) fks)
frekvenssi content = toList (fromListWith (+) [(c, 1) | c <- content])

chunks n xs 
  | n <= length xs = fst (splitAt n xs) : chunks n (tail xs)
  | otherwise      = []

nth rand r (x:y:zs)
  | r <= rand = x
  | otherwise = nth rand (r*r) (y:zs)
nth rand r (x:[]) = x

newLetter rands xs =
  fst (head dw)
  where
    r1 = head rands
    r2 = head (drop 1 rands)
    dw = dropWhile (\x -> snd x < r1) fj
    fj = fromJust (nth r2 0.50 [x | x <- xs, x /= Nothing])

growing xs = 
  zip b [sum (take n c)  / tot :: Double | n <- [1..length c]]
  where
    tot = sum c 
    (b,c) = unzip xs

generate fksGrow accum rands = 
  c : generate fksGrow newAccum newRands
  where
    newRands = drop 2 rands
    newAccum = lastN 6 (accum ++ [c])
    xs = found accum fksGrow
    c = newLetter (take 2 rands) xs 

putCharF c = do
  (putStr . charToString) c
  hFlush stdout

cutGenerate piece accum = do
  g <- getStdGen
  let
   xs = [7,6..1]
   chks = [chunks x piece | x <- xs]
   fks  = [frekvenssi x | x <- chks]
   ics = [initchks x | x <- chks]
   fks2 = [freks2 x | x <- ics]
   fksGrow =  [[(a, growing (frekvenssi b)) | (a,b) <- fks] | fks <- fks2]
   rands = randomRs (0.0,1.0) g :: [Double]
   generated = generate fksGrow accum rands
  mapM_ putCharF generated

charToString :: Char -> String
charToString c = [c]

sampleStart = 0
sampleSize  = 25000
fileName = "journey-to-centre-of-earth.txt"

main = do
  content <- readFile fileName
  let
    piece2 = drop sampleStart content
    startAccum = take 6 piece2
    piece = take sampleSize piece2
  mapM_ putCharF startAccum
  cutGenerate piece startAccum



