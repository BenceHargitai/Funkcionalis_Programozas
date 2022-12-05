module Lesson where

fact :: Int -> Int
fact 0 = 1
fact n = fact (n-1) * n

pow :: Int -> Int -> Int
pow 0 _ = 0
pow _ 0 = 1
pow a 1 = a
pow a b = a * pow a (b-1)

range :: Int -> Int -> [Int]
range a b
    | a > b = []
    | otherwise = a : range (a+1) b

minimum' :: [Int] -> Int
minimum' [a] = a
minimum' (x:xs) = min x (minimum' xs)

everySecond :: [a] -> [a]
everySecond [] = []
everySecond [x] = []
everySecond (_:x:xs) = x : everySecond xs









length' :: [Int] -> Int
length' [] = 0
length' (x:xs) = 1 + length' xs

max' :: [Int] -> Int
max' [x] = x
max' (x:xs) = max x (max' xs)

hatvany :: Int -> Int -> Int
hatvany 0 _ = 0
hatvany a 1 = a
hatvany a b = a * hatvany a (b-1)


putIntoList :: a -> [a]
putIntoList x  =  [x]


headTail :: [a] -> (a, [a])
headTail [x] = (x,[])
headTail l = (head l, tail l)

doubleHead :: [a] -> [b] -> (a, b)
doubleHead a b = (head a, head b)

doubleAll :: [Int] -> [Int]
doubleAll [] = []
doubleAll (x:xs) = x*2 : doubleAll xs

isLonger :: [a] -> [b] -> Bool
isLonger [] l = False
isLonger [] [] = False
isLonger l [] = True
isLonger (x:xs) (y:ys) = isLonger xs ys

sumTomb :: [Int] -> [Int] -> [Int]
sumTomb [] l = l
sumTomb l [] = l
sumTomb (x:xs) (y:ys) = x+y : sumTomb xs ys

getNth :: [a] -> Int -> a 
getNth l 1 = head l
getNth l n = getNth (tail l) (n-1)







isLongerThanSecond :: [a] -> [b] -> Bool
isLongerThanSecond [] l = False
isLongerThanSecond [] [] = False
isLongerThanSecond l [] = True
isLongerThanSecond (x:xs) (y:ys) = isLongerThanSecond xs ys




-- fact :: Integer -> Integer
-- fact 0 = 1
-- fact n = n * factorial (n - 1)

-- pow :: Integer -> Integer -> Integer
-- pow _ 0 = 1
-- pow x n = x * power x (n - 1)

-- range :: Integer -> Integer -> [Integer]
-- range a b
--   | a > b = []
--   | otherwise = a : range (a + 1) b

-- minimum' :: [Integer] -> Integer
-- minimum' [] = error "empty list"
-- minimum' [x] = x
-- minimum' (x:xs) = min x (minimum' xs)

-- everySecond :: [a] -> [a]
-- everySecond [] = []
-- everySecond [x] = [x]
-- everySecond (x:_:xs) = x : everySecond xs