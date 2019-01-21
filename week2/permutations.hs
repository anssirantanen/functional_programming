
foldUnique:: [[Char]] -> [Char]
foldUnique strings =
   unique (foldl (\x y -> x++y) [] strings) 

generate_pairs str =
    mapM (const str) [1..2]

has :: (Eq a) => [a] -> a -> Bool
has [] _ = False
has (x:xs) a
  | x == a    = True
  | otherwise = has xs a

unique :: (Eq a) => [a] -> [a]
unique [] = []
unique (x:xs)
  | has xs x  = unique xs
  | otherwise = x : unique xs

pairs :: [[Char]] -> [[Char]]
pairs words =
    generate_pairs(foldUnique words)