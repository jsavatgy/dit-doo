# dit-doo

A given text gives us the probabilities of letters to appear. Given the last six letters, what will the next one be?

## Collecting chunks

Lets first collect all the chunks of length `[7,6..1]` from a given text.

```haskell
str = "sinus aestuum"
ns = [7,6..1]

chunks n xs 
  | n <= length xs = fst (splitAt n xs) : chunks n (tail xs)
  | otherwise      = []

chks = [chunks x str | x <- ns]
```

Now

```haskell
> str
"sinus aestuum"
> mapM_ (putStrLn . show) chks
["sinus a","inus ae","nus aes","us aest","s aestu"," aestuu","aestuum"]
["sinus ","inus a","nus ae","us aes","s aest"," aestu","aestuu","estuum"]
["sinus","inus ","nus a","us ae","s aes"," aest","aestu","estuu","stuum"]
["sinu","inus","nus ","us a","s ae"," aes","aest","estu","stuu","tuum"]
["sin","inu","nus","us ","s a"," ae","aes","est","stu","tuu","uum"]
["si","in","nu","us","s "," a","ae","es","st","tu","uu","um"]
["s","i","n","u","s"," ","a","e","s","t","u","u","m"]
```

## Calculating frequency

Using a list comprehension we get a list of tuples.

```haskell
> str
"sinus aestuum"
> let t = [(c, 1) | c <- str]
> t
[('s',1),('i',1),('n',1),('u',1),('s',1),(' ',1),('a',1),('e',1),('s',1),('t',1),('u',1),('u',1),('m',1)]
```

A possible data type for representing the frequency is `Map` from the module `Data.Map`. We use the function `fromListWith` to collect all the tuples. The function gets as its first parameter a function that is used between the values when the keys are equal. In this case we use the function `(+)`. We finally convert the map back to a list with the function `toList`.

```haskell
> (toList . fromListWith (+)) t
[(' ',1),('a',1),('e',1),('i',1),('m',1),('n',1),('s',3),('t',1),('u',3)]
```

The same works quite well when reading the text from a file.

```haskell
> content <- readFile fileName
> frequency content
[('\n',5234),('\r',5234),(' ',83311),('!',365),('"',2648),('\'',166),('(',15),(')',15),('*',15),('+',1),(',',6678),('-',1180),('.',4881),('/',35),('0',15),('1',46),('2',37),('3',28),('4',20),('5',14),('6',13),('7',10),('8',13),('9',11),(':',49),(';',411),('<',70),('>',70),('?',443),('A',712),('B',298),('C',179),('D',145),('E',295),('F',147),('G',132),('H',723),('I',2649),('J',41),('K',33),('L',135),('M',387),('N',293),('O',227),('P',318),('Q',16),('R',172),('S',410),('T',1071),('U',84),('V',41),('W',613),('X',1),('Y',146),('Z',2),('[',16),(']',16),('a',29744),('b',4992),('c',10664),('d',15785),('e',48156),('f',9337),('g',6918),('h',20639),('i',22996),('j',353),('k',2609),('l',15644),('m',9357),('n',25238),('o',28939),('p',6301),('q',348),('r',22354),('s',23226),('t',33047),('u',11846),('v',3947),('w',8721),('x',821),('y',7594),('z',209),('\163',1),('\65279',1)]
```

## Random letters

Next we generate random numbers by the functions found in module `System.Random`, thus we must first import the module.

```haskell
import System.Random
```

We decide to produce an endless list of random values between `0.0` and `1.0` of type `Double` and later scale the values to match the needed interval. During testing we may want to always produce the same sequence of numbers. This would be done by initializing the standard random generator to a specified value (like `42` in our out-commented example).

```haskell
rands :: IO [Double]
rands = do
  -- setStdGen (mkStdGen 42)
  g <- getStdGen
  let rs = randomRs (0.0,1.0) g 
  return rs
```

Now we get

```haskell
> rs <- rands
> let t = take 8 rs
> import Text.Printf
> mapM_ (putStrLn . (printf "%.5f")) t
0.39657
0.06528
0.12848
0.27719
0.10342
0.14214
0.96264
0.77976
```

To choose a random letter from a string, we need a random integer. For this, we `scale` and `floor` our list of floating point numbers. Normally the random numbers will be between `0.00` and `0.99`, but occasionally there can be an `1.00`. That number would `floor` differently, so we take care of it by always minimizing the result to be no bigger than the integer `i`.

```haskell
scale :: Int -> Double -> Int
scale i r = min i t
  where
    itd = intToDouble i
    t = floor (r*(itd+1))
```

We do the conversion from `Int` to `Double` via the type `Rational`. The function `fromRational` gets its return type from the type declaration of the function `intToDouble`. 

```haskell
intToDouble :: Int -> Double
intToDouble = fromRational . toRational
```

The function works as expected.

```haskell
> intToDouble 48
48.0
```

Let the string be

```haskell
str = " aieou nstlr"
```

Our function `randomString` takes as its parameters the list of random numbers, the string, and the amount of resulting characters we want.

```haskell
randomString rnds str n = map (str !!) r
  where
    rr = take n rnds
    l = length str-1
    r = map (scale l) rr
```

Let's scale the earlier chosen `8` random numbers to the integer interval from `0` to `11`.

```haskell
> str
" aieou nstlr"
> let l = length str-1
> l
11
> let r = map (scale l) t
> r
[4,0,1,3,1,1,11,9]
```

Mapping them over the string `str` they become

```haskell
>  map (str !!) r
"o aeaart"
```

Taking `540` of them is no problem.

```haskell
> randomString rs str 540
"o aeaarttria eeo o aeaorelutolusi oin sanntinorlir nraiaauss isareal utitieoanlesle  salrnarel lnsstut onos s e tneorstsat irassore ontul o etu a rlneta llaanreo  reutn eu roertsto  eirrnitlersse atasnnnt usi usniles su aa  ttsoonnriooluasuoart uauneuaaoustasrn u una nealuasor esaotua an iatasl  s un   aetreliselrisaetn iotlta sr su otnaat n urnesl   trrnirul su tnine nnaneio  r  isulss  seeua t  rrso ntau lesno a nntt u tooarrisisslsnn natoilo   rarsit sueeiaueseeoiolisuunu rlsurnn otaoeooelae nn  r oittsis ns ou el srnuleiutnlnre  t"
```

## Building a map

We import the module `Data.Map` and use the prefix `Map.` for its functions to avoid conflicts with `Prelude`.

```haskell
import qualified Data.Map as Map
```

We flatten a list of lists to a list with the function `concat`.

```haskell
> concat [[1,2],[3,4]]
[1,2,3,4]
```

Here we do it for our `chunks`.

```haskell
chks str = concat [chunks x str | x <- ns]
```

We split a chunk into root and extension. 

```haskell
> let s = "umorum"
> let (root,ext) = (init s, [last s])
> root
"umoru"
> ext
"m"
```

The root becomes the key in our map. 

```haskell
roots chks = 
  Map.fromListWith tac t
  where 
    t = [(init s, [last s]) | s <- chks]
```

With the function `tac` we form the value corresponding a key. For now, we simply collect the extensions as a list.

```haskell
tac a b = b ++ a
```

From the string `str` we get the map. When the root is empty, the value becomes a list of all the letters. When the key is `"a"`, the value becomes the list `"rn"`, because those are the letters following the string `"a"` in the string `"mare anguis"`.

```haskell
> str
"mare anguis"
> roots (chks str)
fromList [("","mare anguis"),(" ","a"),(" a","n"),(" an","g"),(" ang","u"),(" angu","i"),(" angui","s"),("a","rn"),("an","g"),("ang","u"),("angu","i"),("angui","s"),("ar","e"),("are"," "),("are ","a"),("are a","n"),("are an","g"),("e"," "),("e ","a"),("e a","n"),("e an","g"),("e ang","u"),("e angu","i"),("g","u"),("gu","i"),("gui","s"),("i","s"),("m","a"),("ma","r"),("mar","e"),("mare"," "),("mare ","a"),("mare a","n"),("n","g"),("ng","u"),("ngu","i"),("ngui","s"),("r","e"),("re"," "),("re ","a"),("re a","n"),("re an","g"),("re ang","u"),("u","i"),("ui","s")]
```

We could do a simple lookup to find out what comes after `"ng"`.

```haskell
> let r = roots (chks str)
> Map.lookup "ng" r
Just "u"
```

## Singleton and rootmap

We want to build a nested tree from our structure. We get an element, say, `("a",1)`, then `("b",2)`, then `("c",3)`. We know the function `union` from the module `Map`, which combines two trees together. Thus we need to make a tree out of our element. This is done by the function `singleton`. It forms a single-leaf tree out of one element. These single-element trees can be combined by the function `union`. 

```haskell
> let s1 = Map.singleton "a" 1
> let s2 = Map.singleton "b" 2
> let s3 = Map.singleton "c" 3
> let u1 = s1 `Map.union` s2 `Map.union` s3
> u1
fromList [("a",1),("b",2),("c",3)]
> :type u1
u1 :: Num a => Map.Map [Char] a
```

To collect a frequency tree, we use the function `unionWith (+)`.

```haskell
rootmap str = 
  Map.fromListWith (Map.unionWith (+)) t
  where 
    t = [(init s, Map.singleton (last s) 1) | s <- chks str]
    chks str = concat [chunks x str | x <- ns]
```

Now

```haskell
> let r = rootmap "lacus"
> r
fromList [("",fromList [('a',1),('c',1),('l',1),('s',1),('u',1)]),("a",fromList [('c',1)]),("ac",fromList [('u',1)]),("acu",fromList [('s',1)]),("c",fromList [('u',1)]),("cu",fromList [('s',1)]),("l",fromList [('a',1)]),("la",fromList [('c',1)]),("lac",fromList [('u',1)]),("lacu",fromList [('s',1)]),("u",fromList [('s',1)])]
```

The functions `showTree` and `showTreeWith` return the hierarchical view to our tree. For this we define the function `treeV`.

```haskell
treeV r = Map.showTreeWith (\k x -> show (k,x)) True False r
```

Used with the function `putStrLn` it outputs the tree structure.

```haskell
> putStrLn (treeV r)
("la",fromList [('c',1)])
+--("acu",fromList [('s',1)])
|  +--("a",fromList [('c',1)])
|  |  +--("",fromList [('a',1),('c',1),('l',1),('s',1),('u',1)])
|  |  +--("ac",fromList [('u',1)])
|  +--("cu",fromList [('s',1)])
|     +--("c",fromList [('u',1)])
|     +--("l",fromList [('a',1)])
+--("lacu",fromList [('s',1)])
   +--("lac",fromList [('u',1)])
   +--("u",fromList [('s',1)])
```

Here the value `fromList` tells us we have a nested map.


## Logarithmic randomness

We want some randomness in our program. If the length of `accum` is `6`, a good random distribution returns usually all the letters, but in some moments less. We get a nice distribution in logarithmic scale.

We define a function `floorLogBase` to do this.

```haskell
floorLogBase = floor . logBase 0.5
```

Given 100 numbers evenly distributed from interval `[0.00,0.01..1.00]`, the function `floorLogBase` returns a logarithmic distribution.

```haskell
"6655444333333222222222222111111111111111111111111100000000000000000000000000000000000000000000000000"
```

Our function `randLts` would look like

```haskell
randLts rands n = take n r3s
  where
    r3s = concat (map show r2s)
    r2s = map (truncBetween 0 6) r1s
    r1s = map floorLogBase rands
```

We truncate the numbers to be between `a` and `b` by the function `truncBetween`.


```haskell
truncBetween a b x = a `max` x `min` b
```

We get a truncated list.

```haskell
> map (truncBetween 1 6) [0..10]
[1,1,2,3,4,5,6,6,6,6,6]
```

And now we get 540 random numbers between 1 and 6 distributed logarithmically.

```haskell
> rs <- rands
> let r1 = randLts rs 540
> print r1
"001100122000322111020021000003011001013113004112014103000000410601000000010101010420300021123011111011310000040131100000321030100120014121502023020400010012032130443000000020040006111030001003000132000304200212112002001022101003002001010001201010100112001004202001100002322202331110000000221106016202040001024010000010020001110131001010000020003212342021001212001011100020102002103036020010130103203110113002211110300401411000000004010000040101102006010002210000002100020133140010000000002140212222116150011010411034030323010100010211136101"
```

## A letter from a leaf

Let `ea1` be the leaf we want to study.

```haskell
> let ea1 = Map.fromList [(' ',1),('a',1),('b',1),('e',1),('i',2),('m',3),('r',2),('u',1)]
```

We want to know how many letters are there in this leaf. We can use the function `mapAccum` for this. Not only does it give the amount of letters, but it enables us to construct an interval, which we will use to match our random number later and choose the letter accordingly.

```haskell
mapAccumFsum = Map.mapAccum fsum 0 
  where
    fsum a b = (a + b, (a+1,a+b))
```

For our leaf `ea1` this gives

```haskell
> mapAccumFsum ea1
(12,fromList [(' ',(1,1)),('a',(2,2)),('b',(3,3)),('e',(4,4)),('i',(5,6)),('m',(7,9)),('r',(10,11)),('u',(12,12))])
```

We now know that there are 12 letters in this leaf. We construct a random number between 1 and 12, and get the letter by matching the intervals. We can define a function `filterMe` for this.

```haskell
filterMe (d,m) c = Map.filter (\(a,b) -> a<=c && c<=b) m
```

## Dropping letters

We try to find the six-letter string `accum`. If we don't find it, we drop one letter from beginning and try again. In the worst case we are left with empty string `""`, which always matches.

```haskell
randAccum rmap (x,out) = findAccum rmap (drop out x)
findAccum rmap x = lookAccum rmap (Map.lookup x rmap) x
lookAccum rmap (Just x) str = (str,x)
lookAccum rmap Nothing  str = lookAccum rmap (Map.lookup next rmap) next
  where
    next = drop 1 str
```

We test the lookup.

```haskell
> let example = "lacus veris"
> let rmap = rootmap example
> let examples = chunks 6 example
> let l1 = map (findAccum rmap) examples
> mapM_ (putStrLn . show) l1
("lacus ",fromList [('v',1)])
("acus v",fromList [('e',1)])
("cus ve",fromList [('r',1)])
("us ver",fromList [('i',1)])
("s veri",fromList [('s',1)])
("s",fromList [(' ',1)])
```

Using the random numbers, we drop some letters. With short example string this should not affect the results. 

```haskell
> rs <- rands
> let rl = randLts rs
> let l2 = map (randAccum rmap) (zip examples rl)
> mapM_ (putStrLn . show) l2
("us ",fromList [('v',1)])
("v",fromList [('e',1)])
("cus ve",fromList [('r',1)])
("s ver",fromList [('i',1)])
(" veri",fromList [('s',1)])
("s",fromList [(' ',1)])
```

We define a small function `countTest` to test calculations.

```haskell
countTest content = map (\(a,b) -> (a, fst (mapAccumFsum b))) l1
  where
    l1 = map (findAccum rmap) tests
    tests = chunks 4 "matkustus maanx"
    rmap = rootmap example
    example = take 10000 content
```

Reading a text file and calling the function `countText` we get

```haskell
> content <- readFile "matkustus-maan-keskipisteeseen.txt"
> mapM_ (putStrLn . show) (countTest content)
("matk",1)
("ku",51)
("kus",5)
("kust",2)
("stu",10)
("stus",1)
("us ",9)
("us m",1)
(" ma",14)
(" maa",2)
("maan",4)
("",10000)
```

## Random length in action

We again read the file to the variable `content`. We let the `accum` to be 6 letters from the beginning of that content.

```haskell
> content <- readFile "matkustus-maan-keskipisteeseen.txt"
> let accum = take 6 content
> accum
"MATKUS"
```

We create an infinite list of random numbers by the function `rands` and give it the name `rs`. We count a logarithmic distribution from it.

```haskell
> rs <- rands
> let rtls = randLengths rs
> take 10 rtls
[1,0,6,2,0,0,3,1,0,1]
```

This gives the random amount of letters to take off from `accum`: first 1 letter, then 0 letters, then 6 letters, and so on. We get the list `traccums`. We sort it by length and print the reversed result, that is, the longest string first. Like we expected, the logarithmic distribution gives about half of words with full length, half of the rest with one letter less, and so on. Each of them appears only once in the text, only the empty string matches the text for every letter, that is, 10000 times.

```haskell
> let traccums = accumTest content rtls accum
> traccums
[("ATKUS",1),("MATKUS",1),("",10000),("TKUS",1),("MATKUS",1),("MATKUS",1),("KUS",1),("ATKUS",1),("MATKUS",1),("ATKUS",1)]
> let traccumsSorted = sortBy (comparing (\(x,y) -> length x)) traccums 
> traccumsSorted
[("",10000),("KUS",1),("TKUS",1),("ATKUS",1),("ATKUS",1),("ATKUS",1),("MATKUS",1),("MATKUS",1),("MATKUS",1),("MATKUS",1)]
> mapM_ (putStrLn . show) (reverse traccumsSorted)
("MATKUS",1)
("MATKUS",1)
("MATKUS",1)
("MATKUS",1)
("ATKUS",1)
("ATKUS",1)
("ATKUS",1)
("TKUS",1)
("KUS",1)
("",10000)
```

## Random extension in action

We define the functions `extTest` and `extTuple` to test the random extension and form a printable tuple from their results.

```haskell
extTuple accum tree rand = (accum, limit, scaledRand, ks)
  where
    ks = Map.keys (filterMe (limit,i) scaledRand)
    (limit,i) = mapAccumFsum tree
    scaledRand = (scale (limit-1) rand) + 1
    
extTest rands accum = map (\((a,b),c) -> extTuple a b c) l2
  where
    l2 = zip l1 rands
    l1 = map (findAccum rmap) tests
    tests = take 8 (repeat accum)
    rmap = rootmap example
    example = "odii"
```

We give an empty `accum` string `""` and test it for the example content string `"odii"`. The distribution of extensions is what we expected. In the resulting list of tuples first is the `accum`, how many possible extensions it has, then the random integer based on the amount of possibilities and last the corresponding letter. A search tree has its keys in alphabetic order.

```haskell
> let accum = ""
> rs <- rands
> let rexts = extTest rs accum
> mapM_ (putStrLn . show) (sort rexts)
("",4,1,"d")
("",4,1,"d")
("",4,2,"i")
("",4,2,"i")
("",4,3,"i")
("",4,3,"i")
("",4,3,"i")
("",4,4,"o")
```

## Next step


```haskell

```



```haskell

```



```haskell

```

