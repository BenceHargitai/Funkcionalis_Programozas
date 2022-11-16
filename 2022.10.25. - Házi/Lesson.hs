module Homework5 where


geometricSequence :: Num a => a -> a -> [a]
geometricSequence a r = a : geometricSequence (a * r) r

isSorted :: Ord a => [a] -> Bool
isSorted [] = True
isSorted [x] = True
isSorted (x:y:xs) = x <= y && isSorted (y:xs)

runs :: Int -> [a] -> [[a]]
runs n [] = []
runs n l = take n l : runs n (drop n l)

fromTo :: Int -> Int -> [a] -> [a]
fromTo n m l 
    | n>=0 = take (m - n) (drop n l)
    | otherwise = fromTo (n+1) (m) (l)


