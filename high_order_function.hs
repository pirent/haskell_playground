-- High order function
quicksort' [] = []
quicksort' (x:xs) = 
  let smallerSorted = quicksort' (filter (<=x) xs)
      biggerSorted = quicksort' (filter (>x) xs)
  in smallerSorted ++ [x] ++ biggerSorted

largestDivisible = head (filter p [100000, 99999..])
  where p x = x `mod` 3829 == 0

-- - Collatz sequences
chain 1 = [1]
chain n 
  | even n = n : chain (n `div` 2)
  | odd n = n : chain (n*3 + 1)

-- Lamda
numLongChains = length (filter (\xs -> length xs > 15) (map chain [1..100]))

flip'' :: (a -> b -> c) -> b -> a -> c
flip'' f = \x y -> f y x

-- Fold
sumf xs = foldl (\acc x -> acc + x) 0 xs

sumf' :: (Num a) => [a] -> a
sumf' = foldl (+) 0

elemf' :: (Eq a) => a -> [a] -> Bool
elemf' x xs = foldl (\acc y -> if x == y then True else acc) False xs

mapfr f xs = foldr (\x acc -> f x : acc) [] xs
-- fold left with ++ function is much more expensive than :
-- so we usually use right fold when building up new list from a list
mapfl f xs = foldl (\acc x -> acc ++ [f x]) [] xs
