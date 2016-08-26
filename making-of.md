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

## Next step


```haskell

```



```haskell

```



```haskell

```

