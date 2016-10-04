randAccum m x o = findAccum m (drop o x)
  where
    findAccum x = lookAccum m (Map.lookup x m) x
    lookAccum m (Just x) s = (s,x)
    lookAccum m Nothing  s = lookAccum m (Map.lookup n m) n
    n = drop 1 s

(accu,tree) = randAccum m accum rndout

