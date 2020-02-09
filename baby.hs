doubleMe x = x + x
doubleUs x y = doubleMe x + doubleMe y
doubleSmallNumber x = if x > 100 then x else x * 2

boomBangs xs = [ if x < 10 then "BOOM!" else "BANG!" | x <- xs, odd x]

removeNonUppercase st = [c| c <- st, c `elem` ['A'..'Z']]

-- factorial n = product [1..n]
factorial 0 = 1
factorial n = n * factorial (n-1)

circumference r = 2 * pi * r
circumference' r = 2 * pi * r

lucky 7 = "LUCKY SEVEN NUMBER"
lucky x = "Sorry, you are out of luck"

sayMe 1 = "One"
sayMe 2 = "Two"
sayMe 3 = "Three"
sayMe 4 = "Four"
sayMe 5 = "Five"
sayMe x = "Not between one and five"

addVectors (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

first (x,_,_) = x
second (_,y,_) = y
third (_,_,z) = z

head' [] = error "Cannot call head on an empty list, dummy!"
head' (x:_) = x

tell [] = "This list is empty"
tell (x:[]) = "This list has one element: " ++ show x
tell (x:y:[]) = "This list has two elements: " ++ show x ++ " and " ++ show y
tell (x:y:_) = "This list is long. The first two elements are " ++ show x ++ " and " ++ show y

length' [] = 0
length' (_:xs) = 1 + length' xs

sum' [] = 0
sum' (x:xs) = x + sum' xs

capital "" = "Empty string, whoops!"
capital all@(x:xs) = "The first letter of " ++ all ++ " is " ++ [x]

-- Guard
bmiTell weight height
  | bmi <= skinny = "You're underweight, you emo, you!"
  | bmi <= normal = "You're supposedly normal. Pfff, I bet you are ugly!"
  | bmi <= fat = "You're fat! Lose some weight, fatty!"
  | otherwise = "You're a whale! Congratulations!"
  where bmi = weight / height ^ 2
        skinny = 18.5
        normal = 25.0
        fat = 30.0

max' a b
  | a > b = a
  | otherwise = b

a `myCompare` b
  | a > b = GT
  | a < b = LT
  | otherwise = EQ

initials firstname lastname = [f] ++ ". " ++ [l] ++ "."
  where (f:_) = firstname
        (l:_) = lastname

calcBmis xs = [bmi w h | (w, h) <- xs]
  where bmi weight height = weight / height ^ 2

calcBmis' xs = [bmi | (w, h) <- xs, let bmi = w / h ^ 2]
calcBmisFat xs = [bmi | (w, h) <- xs, let bmi = w / h ^ 2, bmi >= 25.0]

-- Let binding expression
cylinder r h =
  let sideArea = 2 * pi * r * h
      topArea = pi * r ^ 2
  in sideArea + 2 * topArea

-- Case expression
describeList xs = "The list is " ++ case xs of [] -> "empty."
                                               [x] -> "singleton list"
                                               xs -> "a longer list"
-- Chapter 6: curried function
multThree x y z = x * y * z
comparedByHundred = compare 100
divideByTen = (/10)
isUpperAlphanum = (`elem` ['A'..'Z'])

applyTwice f x = f (f x)

zipWith' _ [] _ = []
zipWith' _ _ [] = []
zipWith' f (x:xs) (y:ys) = f x y : zipWith' f xs ys

flip' f y x = f x y

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

numLongChains = length (filter isLong (map chain [1..100]))
  where isLong x = length x > 15
