maximum' [] = error "Maximum of an empty list"
maximum' [x] = x
maximum' (x:xs) = max x (maximum' xs)

replicate' n x
  | n <= 0 = []
  | otherwise = x:replicate' (n-1) x

take' n _
  | n <= 0 = []
take' _ [] = []
take' n (x:xs) = x: take' (n-1) xs

reverse' [] = []
reverse' (x:xs) = reverse' xs ++ [x]

zip' [] _ = []
zip' _ [] = []
zip' (x:xs) (y:ys) = (x,y):zip' xs ys

elem' a [] = False
elem' a (x:xs)
  | a == x = True
  | otherwise = elem' a xs

quicksort [] = []
quicksort (x:xs) = 
  let smallerSorted = quicksort [a | a <- xs, a <= x]
      biggerSorted = quicksort [b | b <- xs, b > x]
  in smallerSorted ++ [x] ++ biggerSorted
