import Data.List

main = do
  content <- readFile "journey-to-centre-of-earth.txt"
  let s = sort content
  writeFile "journey-abc.txt" s
