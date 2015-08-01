# dit-doo

Given the last six letters of text, what will the next one be?

## Collecting chunks

Lets first collect all the chunks of length `[1..7]` in a given text.

```
str = "sinus aestuum"

chunks n xs 
  | n <= length xs = fst (splitAt n xs) : chunks n (tail xs)
  | otherwise      = []

chks = [chunks x str | x <- [7,6..1]]

main = do
  mapM_ print chks
```

Running the program gives

```
$ runhaskell chunks-001.hs 
["sinus a","inus ae","nus aes","us aest","s aestu"," aestuu","aestuum"]
["sinus ","inus a","nus ae","us aes","s aest"," aestu","aestuu","estuum"]
["sinus","inus ","nus a","us ae","s aes"," aest","aestu","estuu","stuum"]
["sinu","inus","nus ","us a","s ae"," aes","aest","estu","stuu","tuum"]
["sin","inu","nus","us ","s a"," ae","aes","est","stu","tuu","uum"]
["si","in","nu","us","s "," a","ae","es","st","tu","uu","um"]
["s","i","n","u","s"," ","a","e","s","t","u","u","m"]
```

