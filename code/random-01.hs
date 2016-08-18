import System.Random

intToDouble :: Int -> Double
intToDouble = fromRational . toRational

rands :: IO [Double]
rands = do
  -- setStdGen (mkStdGen 42)
  g <- getStdGen
  let rs = randomRs (0.0,1.0) g 
  return rs

scale :: Int -> Double -> Int
scale i r = min i t
  where
    itd = intToDouble i
    t = floor (r*(itd+1))

str = " aieou nstlr"

randomString rnds str n = map (str !!) r
  where
    rr = take n rnds
    l = length str-1
    r = map (scale l) rr

main = do
  rs <- rands
  print (randomString rs str 540)

