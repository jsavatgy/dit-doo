import System.Random

rands :: IO [Double]
rands = do
  g <- getStdGen
  let rs = randomRs (0.0,1.0) g 
  return rs

floorLogBase = floor . logBase 0.5
truncBetween a b x = a `max` x `min` b

randLts rands n = take n r3s
  where
    r3s = concat (map show r2s)
    r2s = map (truncBetween 0 6) r1s
    r1s = map floorLogBase rands

main = do
  rs <- rands
  let 
    r1 = randLts rs 540
    r2 = randLts [0.00,0.01..1.00] 100
  print r1
  print r2

