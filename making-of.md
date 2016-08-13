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

## What's next?


```haskell

```



```haskell

```



```haskell

```

