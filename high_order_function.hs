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

-- foldl1 works similar to fold without the starting value
-- because it takes the first (last) element of the list as this value
sumf1 :: (Num a) => [a] -> a
sumf1 = foldl1 (+)

-- a bunch of standard library reimplemented with fold
maximumf' :: (Ord a) => [a] -> a
maximumf' xs = foldr1 (\x acc -> if x > acc then x else acc) xs

reversef :: [a] -> [a]
reversef xs = foldl (\acc x -> x : acc) [] xs

productf :: (Num a) => [a] -> a
productf = foldr1 (*)

filterf :: (a -> Bool) -> [a] -> [a]
filterf f = foldr (\x acc -> if f x then x : acc else acc) []

-- head is better implemented in pattern matching, but this just goes to show
headf :: [a] -> a
headf = foldl1 (\x _ -> x)

lastf :: [a] -> a
lastf = foldr1 (\_ x -> x)

-- scanl and scanr are similar to foldl and foldr,
-- except that they reports only the intermediate result of accumulator
-- Example:
-- > scanl (+) 0 [1,2,3,4]
-- [0,1,3,6,10]
-- > scanr (+) 0 [1,2,3,4]
-- [10,9,7,4,0]

sqrtSums :: Int
sqrtSums = length (takeWhile (<1000) (scanl1 (+) (map sqrt [1..]))) + 1

-- function composition
oddSquareSum = sum . takeWhile (<10000) . filter odd . map (^2) $ [1..]
-- in order to avoid a long function composition chain
-- use let bindings to give labels to intermediate results
oddSquareSum' = 
  let oddSquares = filter odd $ map (^2) [1..]
      belowLimit = takeWhile (<10000) oddSquares
  in sum belowLimit
