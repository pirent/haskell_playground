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
